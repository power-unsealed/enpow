use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::{
    spanned::Spanned, Attribute, Data, DataEnum, DeriveInput, Error, Fields, Generics, Ident,
    Variant, Visibility,
};

pub struct EnumInfo {
    pub span: Span,
    pub attributes: Vec<Attribute>,
    pub generics: Generics,
    pub identifier: Ident,
    pub visibility: Visibility,
    pub data: DataEnum,
}

pub trait ExtractEnumInfo {
    fn extract_info(self) -> Result<EnumInfo, Error>;
}

impl ExtractEnumInfo for DeriveInput {
    fn extract_info(self) -> Result<EnumInfo, Error> {
        let span = self.span();

        let data = match self.data {
            Data::Enum(data) => data,
            _ => {
                return Err(Error::new(
                    Span::call_site(),
                    "Can only be derived for enums",
                ));
            }
        };

        Ok(EnumInfo {
            span,
            generics: self.generics,
            attributes: self.attrs,
            identifier: self.ident,
            visibility: self.vis,
            data,
        })
    }
}

pub struct VariantInfo {
    pub identifier: Ident,
    pub snake_case: String,
    pub data_type: TokenStream,
    pub type_def: Option<TokenStream>,
    pub pattern: TokenStream,
    pub construction: TokenStream,
}

impl VariantInfo {
    pub fn new(
        data_type: TokenStream,
        type_def: Option<TokenStream>,
        pattern: TokenStream,
        construction: TokenStream,
        identifier: Ident,
    ) -> VariantInfo {
        VariantInfo {
            snake_case: identifier.to_string().to_snake_case(),
            identifier,
            data_type,
            type_def,
            pattern,
            construction,
        }
    }

    pub fn build_base(&self) -> TokenStream {
        let snake_case = &self.snake_case;
        let data_type = &self.data_type;
        let pattern = &self.pattern;
        let construction = &self.construction;

        let fn_ident = Ident::new(&snake_case, Span::call_site());

        quote! {
            fn #fn_ident(self) -> Option< #data_type > {
                match self {
                    #pattern => Some(#construction),
                    _ => None,
                }
            }
        }
    }

    pub fn build_is(&self) -> TokenStream {
        let snake_case = &self.snake_case;
        let pattern = &self.pattern;

        let fn_ident = format_ident!("is_{snake_case}");

        quote! {
            fn #fn_ident(&self) -> bool {
                match self {
                    #pattern => true,
                    _ => false,
                }
            }
        }
    }

    pub fn build_unwrap(&self, parent: &EnumInfo) -> TokenStream {
        let snake_case = &self.snake_case;
        let data_type = &self.data_type;
        let pattern = &self.pattern;
        let construction = &self.construction;

        let fn_ident = format_ident!("unwrap_{snake_case}");
        let panic_msg = format!(
            "Failed unwrapping to {}::{}. Unexpected variant",
            parent.identifier, self.identifier,
        );

        quote! {
            fn #fn_ident(self) -> #data_type {
                match self {
                    #pattern => #construction,
                    _ => panic!(#panic_msg),
                }
            }
        }
    }

    pub fn build_expect(&self) -> TokenStream {
        let snake_case = &self.snake_case;
        let data_type = &self.data_type;
        let pattern = &self.pattern;
        let construction = &self.construction;

        let fn_ident = format_ident!("expect_{snake_case}");

        quote! {
            fn #fn_ident(self, msg: &str) -> #data_type {
                match self {
                    #pattern => #construction,
                    _ => panic!("{msg}"),
                }
            }
        }
    }

    pub fn build_unwrap_or(&self) -> TokenStream {
        let snake_case = &self.snake_case;
        let data_type = &self.data_type;
        let pattern = &self.pattern;
        let construction = &self.construction;

        let fn_ident = format_ident!("unwrap_{snake_case}_or");

        quote! {
            fn #fn_ident(self, default: #data_type) -> #data_type {
                match self {
                    #pattern => #construction,
                    _ => default,
                }
            }
        }
    }
}

pub trait ExtractVariantInfo {
    fn extract_info(self, parent: &EnumInfo) -> VariantInfo;
}

impl ExtractVariantInfo for Variant {
    fn extract_info(self, parent: &EnumInfo) -> VariantInfo {
        let enum_ident = &parent.identifier;
        let identifier = self.ident;

        match self.fields {
            // Variant without data
            Fields::Unit => VariantInfo::new(
                quote! { () },
                None,
                quote! { #enum_ident::#identifier },
                quote! { () },
                identifier,
            ),

            // Variant with exactly one unnamed field
            Fields::Unnamed(tuple) if tuple.unnamed.len() == 1 => {
                let single = &tuple.unnamed[0].ty;
                VariantInfo::new(
                    quote! { #single },
                    None,
                    quote! { #enum_ident::#identifier( f0 ) },
                    quote! { f0 },
                    identifier,
                )
            }

            // Variant with more than one unnamed field
            Fields::Unnamed(tuple) => {
                let fields: Vec<_> = tuple
                    .unnamed
                    .iter()
                    .enumerate()
                    .map(|(i, _)| format_ident!("f{i}"))
                    .collect();

                VariantInfo::new(
                    quote! { #tuple },
                    None,
                    quote! { #enum_ident::#identifier( #(#fields),* ) },
                    quote! { ( #(#fields),* ) },
                    identifier,
                )
            }

            // Variant with named fields
            Fields::Named(fields) => {
                let visibility = &parent.visibility;
                let generics = &parent.generics;
                let (_, gen_short, gen_where) = parent.generics.split_for_impl();
                let type_ident = format_ident!("{enum_ident}{identifier}");
                let data_type = quote! { #type_ident #gen_short };

                let (field_idents, field_types): (Vec<_>, Vec<_>) = fields
                    .named
                    .into_iter()
                    .map(|field| (field.ident.unwrap(), field.ty))
                    .unzip();

                let fields: Vec<_> = field_idents
                    .iter()
                    .zip(field_types.iter())
                    .map(|(i, t)| quote! { pub #i: #t })
                    .collect();

                VariantInfo::new(
                    data_type,
                    Some(quote! {
                        #visibility struct #type_ident #generics #gen_where { #(#fields),* }
                    }),
                    quote! { #enum_ident::#identifier { #(#field_idents),* } },
                    quote! { #type_ident { #(#field_idents),* } },
                    identifier,
                )
            }
        }
    }
}

pub trait SnakeCase {
    fn to_snake_case(self) -> String;
}

impl<T: AsRef<str>> SnakeCase for T {
    fn to_snake_case(self) -> String {
        // We probably wont need much more than two underscores...
        let mut output = String::with_capacity(self.as_ref().len() + 2);

        // Turn the input into snake case char by char
        let mut last_uppercase = true;
        let mut last_underscore = false;
        for char in self.as_ref().chars() {
            if char.is_uppercase() {
                // If there is an uppercase char that is not preceeded by another
                // uppercase char or an underscore, add an underscore in front of it
                if !last_uppercase && !last_underscore {
                    output.push('_');
                }

                // Add the lower case version of it to the output
                for char in char.to_lowercase() {
                    output.push(char);
                }

                last_uppercase = true;
            } else {
                output.push(char);

                last_uppercase = false;
            }

            last_underscore = char == '_';
        }

        output
    }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
/// Tests
///////////////////////////////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod tests {
    use crate::helper::SnakeCase;

    #[test]
    fn to_snake_case() {
        assert_eq!("IpAddress".to_snake_case(), "ip_address");
        assert_eq!("TCP".to_snake_case(), "tcp");
        assert_eq!("snake_case".to_snake_case(), "snake_case");
        assert_eq!("HOME_IP".to_snake_case(), "home_ip");
    }
}
