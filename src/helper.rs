use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::{
    spanned::Spanned, Attribute, Data, DataEnum, DeriveInput, Error, Field, Fields, FieldsNamed,
    FieldsUnnamed, GenericParam, Generics, Ident, Lifetime, LifetimeDef, Variant, Visibility,
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
    /// Data type of variant data in self, ref, and mut version
    pub data_type: (TokenStream, TokenStream, TokenStream),
    pub type_defs: Vec<TokenStream>,
    pub pattern: TokenStream,
    /// Construction of variant data in self, ref, and mut version
    pub construction: (TokenStream, TokenStream, TokenStream),
}

impl VariantInfo {
    pub fn new(
        data_type: (TokenStream, TokenStream, TokenStream),
        type_defs: Vec<TokenStream>,
        pattern: TokenStream,
        construction: (TokenStream, TokenStream, TokenStream),
        identifier: Ident,
    ) -> VariantInfo {
        VariantInfo {
            snake_case: identifier.to_string().to_snake_case(),
            identifier,
            data_type,
            type_defs,
            pattern,
            construction,
        }
    }

    pub fn from_unit(identifier: Ident, parent: &EnumInfo) -> Result<VariantInfo, Error> {
        let enum_ident = &parent.identifier;

        Ok(VariantInfo::new(
            (quote! { () }, quote! { () }, quote! { () }),
            Vec::new(),
            quote! { #enum_ident::#identifier },
            (quote! { () }, quote! { () }, quote! { () }),
            identifier,
        ))
    }

    pub fn from_field(
        identifier: Ident,
        field: Field,
        parent: &EnumInfo,
    ) -> Result<VariantInfo, Error> {
        let enum_ident = &parent.identifier;

        let single = &field.ty;
        Ok(VariantInfo::new(
            (
                quote! { #single },
                quote! { & #single },
                quote! { &mut #single },
            ),
            Vec::new(),
            quote! { #enum_ident::#identifier( f0 ) },
            (quote! { f0 }, quote! { f0 }, quote! { f0 }),
            identifier,
        ))
    }

    pub fn from_unnamed(
        identifier: Ident,
        tuple: FieldsUnnamed,
        parent: &EnumInfo,
    ) -> Result<VariantInfo, Error> {
        let enum_ident = &parent.identifier;
        let fields: Vec<_> = tuple
            .unnamed
            .iter()
            .enumerate()
            .map(|(i, _)| format_ident!("f{i}"))
            .collect();
        let construction = quote! { ( #(#fields),* ) };

        let mut ref_tuple = tuple.clone();
        for field in ref_tuple.unnamed.iter_mut() {
            let ftype = &field.ty;
            field.ty = syn::parse2(quote! { & #ftype })?;
        }

        let mut mut_tuple = tuple.clone();
        for field in mut_tuple.unnamed.iter_mut() {
            let ftype = &field.ty;
            field.ty = syn::parse2(quote! { &mut #ftype })?;
        }

        Ok(VariantInfo::new(
            (
                quote! { #tuple },
                quote! { #ref_tuple },
                quote! { #mut_tuple },
            ),
            Vec::new(),
            quote! { #enum_ident::#identifier #construction },
            (construction.clone(), construction.clone(), construction),
            identifier,
        ))
    }

    pub fn from_named(
        identifier: Ident,
        fields: FieldsNamed,
        parent: &EnumInfo,
    ) -> Result<VariantInfo, Error> {
        let enum_ident = &parent.identifier;
        let visibility = &parent.visibility;
        let (field_idents, field_types): (Vec<_>, Vec<_>) = fields
            .named
            .into_iter()
            .map(|field| (field.ident.unwrap(), field.ty))
            .unzip();
        let constructor = quote! { { #(#field_idents),* } };
        let (_, gen_short, gen_where) = parent.generics.split_for_impl();

        // Value struct
        let type_ident = format_ident!("{enum_ident}{identifier}");
        let generics = &parent.generics;
        let data_type = quote! { #type_ident #gen_short };
        let fields: Vec<_> = field_idents
            .iter()
            .zip(field_types.iter())
            .map(|(i, t)| quote! { pub #i: #t })
            .collect();
        let type_def = quote! {
            #visibility struct #type_ident #generics #gen_where { #(#fields),* }
        };
        let data_constr = quote! { #type_ident #constructor };

        // Build generics for ref and mut
        let lifetime_name = format!("'{}", type_ident.to_string().to_snake_case());
        let lifetime = Lifetime::new(&lifetime_name, Span::call_site());
        let mut gen_params = generics.params.clone();
        gen_params.push(GenericParam::Lifetime(LifetimeDef::new(lifetime.clone())));
        let mut ref_generics = generics.clone();
        ref_generics.params = gen_params;

        // Ref struct
        let ref_ident = format_ident!("{type_ident}Ref");
        let ref_type = quote! { #ref_ident #gen_short };
        let fields_ref: Vec<_> = field_idents
            .iter()
            .zip(field_types.iter())
            .map(|(i, t)| quote! { pub #i: &#lifetime #t })
            .collect();
        let ref_def = quote! {
            #visibility struct #ref_ident #ref_generics #gen_where { #(#fields_ref),* }
        };
        let ref_constr = quote! { #ref_ident #constructor };

        // Mut struct
        let mut_ident = format_ident!("{type_ident}RefMut");
        let mut_type = quote! { #mut_ident #gen_short };
        let fields_mut: Vec<_> = field_idents
            .iter()
            .zip(field_types.iter())
            .map(|(i, t)| quote! { pub #i: &#lifetime mut #t })
            .collect();
        let mut_def = quote! {
            #visibility struct #mut_ident #ref_generics #gen_where { #(#fields_mut),* }
        };
        let mut_constr = quote! { #mut_ident #constructor };

        Ok(VariantInfo::new(
            (data_type, ref_type, mut_type),
            vec![type_def, ref_def, mut_def],
            quote! { #enum_ident::#identifier #constructor },
            (data_constr, ref_constr, mut_constr),
            identifier,
        ))
    }

    pub fn build_base(&self) -> TokenStream {
        let snake_case = &self.snake_case;
        let data_type = &self.data_type.0;
        let pattern = &self.pattern;
        let construction = &self.construction.0;

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

    pub fn build_base_as_ref(&self) -> TokenStream {
        let snake_case = &self.snake_case;
        let data_type = &self.data_type.1;
        let pattern = &self.pattern;
        let construction = &self.construction.1;

        let fn_ident = format_ident!("{snake_case}_as_ref");

        quote! {
            fn #fn_ident(&self) -> Option< #data_type > {
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
        let data_type = &self.data_type.0;
        let pattern = &self.pattern;
        let construction = &self.construction.0;

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
        let data_type = &self.data_type.0;
        let pattern = &self.pattern;
        let construction = &self.construction.0;

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
        let data_type = &self.data_type.0;
        let pattern = &self.pattern;
        let construction = &self.construction.0;

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

    pub fn build_unwrap_or_else(&self) -> TokenStream {
        let snake_case = &self.snake_case;
        let data_type = &self.data_type.0;
        let pattern = &self.pattern;
        let construction = &self.construction.0;

        let fn_ident = format_ident!("unwrap_{snake_case}_or_else");

        quote! {
            fn #fn_ident(self, f: impl FnOnce(Self) -> #data_type) -> #data_type {
                match self {
                    #pattern => #construction,
                    some => f(some),
                }
            }
        }
    }
}

pub trait ExtractVariantInfo {
    fn extract_info(self, parent: &EnumInfo) -> Result<VariantInfo, Error>;
}

impl ExtractVariantInfo for Variant {
    fn extract_info(self, parent: &EnumInfo) -> Result<VariantInfo, Error> {
        let identifier = self.ident;

        match self.fields {
            // Variant without data
            Fields::Unit => VariantInfo::from_unit(identifier, parent),

            // Variant with unnamed fields
            Fields::Unnamed(tuple) => {
                if tuple.unnamed.len() == 1 {
                    let field = tuple.unnamed[0].clone();
                    VariantInfo::from_field(identifier, field, parent)
                } else {
                    VariantInfo::from_unnamed(identifier, tuple, parent)
                }
            }

            // Variant with named fields
            Fields::Named(fields) => VariantInfo::from_named(identifier, fields, parent),
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
