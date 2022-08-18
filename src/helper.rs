use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};
use std::collections::HashSet;
use syn::{
    parenthesized, parse::Parse, parse::ParseStream, punctuated::Punctuated, spanned::Spanned,
    Attribute, Data, DataEnum, DeriveInput, Error, Field, Fields, FieldsNamed, FieldsUnnamed,
    GenericParam, Generics, Ident, Lifetime, LifetimeDef, Path, Token, Variant, Visibility,
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum MethodType {
    All,
    Variant,
    IsVariant,
    VariantAsRef,
    UnwrapVariant,
    ExpectVariant,
}

impl MethodType {
    pub fn from_attribute(attribute: TokenStream) -> Result<HashSet<MethodType>, Error> {
        let info: EnpowAttributeInfo = syn::parse2(attribute)?;
        Ok(info.methods)
    }

    pub fn needs_self_type(&self) -> bool {
        match self {
            MethodType::All => true,
            MethodType::Variant => true,
            MethodType::IsVariant => false,
            MethodType::VariantAsRef => false,
            MethodType::UnwrapVariant => true,
            MethodType::ExpectVariant => true,
        }
    }

    pub fn needs_ref_type(&self) -> bool {
        match self {
            MethodType::All => true,
            MethodType::Variant => false,
            MethodType::IsVariant => true,
            MethodType::VariantAsRef => true,
            MethodType::UnwrapVariant => false,
            MethodType::ExpectVariant => false,
        }
    }

    pub fn needs_mut_type(&self) -> bool {
        match self {
            MethodType::All => true,
            MethodType::Variant => false,
            MethodType::IsVariant => false,
            MethodType::VariantAsRef => true,
            MethodType::UnwrapVariant => false,
            MethodType::ExpectVariant => false,
        }
    }
}

pub struct EnpowAttributeInfo {
    methods: HashSet<MethodType>,
}

impl Parse for EnpowAttributeInfo {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let mut items: Punctuated<_, Token![,]> = input.parse_terminated(Ident::parse)?;
        let mut methods = HashSet::new();

        // If there are no arguments, insert a fake "All"
        if items.is_empty() {
            items.push(Ident::new("All", Span::call_site()));
        }

        for item in items {
            match item.to_string().as_str() {
                "All" => {
                    methods.insert(MethodType::All);
                }
                "Var" => {
                    methods.insert(MethodType::Variant);
                }
                "IsVar" => {
                    methods.insert(MethodType::IsVariant);
                }
                "VarAsRef" => {
                    methods.insert(MethodType::VariantAsRef);
                }
                "UnwrapVar" => {
                    methods.insert(MethodType::UnwrapVariant);
                }
                "ExpectVar" => {
                    methods.insert(MethodType::ExpectVariant);
                }
                some => {
                    return Err(Error::new_spanned(
                        item,
                        format!("Unknown argument `{some}`"),
                    ));
                }
            }
        }

        Ok(EnpowAttributeInfo { methods })
    }
}

pub struct VarDeriveAttributeInfo {
    pub derives: Vec<Path>,
}

impl Parse for VarDeriveAttributeInfo {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        if input.is_empty() {
            Ok(VarDeriveAttributeInfo {
                derives: Vec::new(),
            })
        } else {
            // Parse the derive arguments in parenthesis
            let content;
            parenthesized!(content in input);
            let derives: Punctuated<_, Token![,]> = content.parse_terminated(Path::parse)?;

            Ok(VarDeriveAttributeInfo {
                derives: derives.into_iter().collect(),
            })
        }
    }
}

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
    /// Type definition of variant data in self, ref, and mut version
    pub type_def: (
        Option<TokenStream>,
        Option<TokenStream>,
        Option<TokenStream>,
    ),
    pub pattern: TokenStream,
    /// Construction of variant data in self, ref, and mut version
    pub construction: (TokenStream, TokenStream, TokenStream),
}

impl VariantInfo {
    pub fn new(
        data_type: (TokenStream, TokenStream, TokenStream),
        type_def: (
            Option<TokenStream>,
            Option<TokenStream>,
            Option<TokenStream>,
        ),
        pattern: TokenStream,
        construction: (TokenStream, TokenStream, TokenStream),
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

    pub fn from_unit(identifier: Ident, parent: &EnumInfo) -> Result<VariantInfo, Error> {
        let enum_ident = &parent.identifier;

        Ok(VariantInfo::new(
            (quote! { () }, quote! { () }, quote! { () }),
            (None, None, None),
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
            (None, None, None),
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
            (None, None, None),
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

        // Self struct
        let type_ident = format_ident!("{enum_ident}{identifier}");
        let generics = &parent.generics;
        let self_type = quote! { #type_ident #gen_short };
        let fields: Vec<_> = field_idents
            .iter()
            .zip(field_types.iter())
            .map(|(i, t)| quote! { pub #i: #t })
            .collect();
        let self_def = quote! {
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
        let ref_fields: Vec<_> = field_idents
            .iter()
            .zip(field_types.iter())
            .map(|(i, t)| quote! { pub #i: &#lifetime #t })
            .collect();
        let ref_def = quote! {
            #visibility struct #ref_ident #ref_generics #gen_where { #(#ref_fields),* }
        };
        let ref_constr = quote! { #ref_ident #constructor };

        // Mut struct
        let mut_ident = format_ident!("{type_ident}Mut");
        let mut_type = quote! { #mut_ident #gen_short };
        let mut_fields: Vec<_> = field_idents
            .iter()
            .zip(field_types.iter())
            .map(|(i, t)| quote! { pub #i: &#lifetime mut #t })
            .collect();
        let mut_def = quote! {
            #visibility struct #mut_ident #ref_generics #gen_where { #(#mut_fields),* }
        };
        let mut_constr = quote! { #mut_ident #constructor };

        Ok(VariantInfo::new(
            (self_type, ref_type, mut_type),
            (Some(self_def), Some(ref_def), Some(mut_def)),
            quote! { #enum_ident::#identifier #constructor },
            (data_constr, ref_constr, mut_constr),
            identifier,
        ))
    }

    pub fn build_method_types(&self, parent: &EnumInfo, types: &[MethodType]) -> Vec<TokenStream> {
        let mut methods = Vec::new();
        for t in types {
            match t {
                MethodType::All => {
                    methods.push(self.build_variant());
                    methods.push(self.build_is());
                    methods.push(self.build_is_and());
                    methods.push(self.build_as_ref());
                    methods.push(self.build_as_mut());
                    methods.push(self.build_unwrap(parent));
                    methods.push(self.build_unwrap_or());
                    methods.push(self.build_unwrap_or_else());
                    methods.push(self.build_expect());
                }
                MethodType::Variant => {
                    methods.push(self.build_variant());
                }
                MethodType::IsVariant => {
                    methods.push(self.build_is());
                    methods.push(self.build_is_and());
                }
                MethodType::VariantAsRef => {
                    methods.push(self.build_as_ref());
                    methods.push(self.build_as_mut());
                }
                MethodType::UnwrapVariant => {
                    methods.push(self.build_unwrap(parent));
                    methods.push(self.build_unwrap_or());
                    methods.push(self.build_unwrap_or_else());
                }
                MethodType::ExpectVariant => {
                    methods.push(self.build_expect());
                }
            }
        }
        methods
    }

    pub fn build_variant(&self) -> TokenStream {
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

    pub fn build_as_ref(&self) -> TokenStream {
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

    pub fn build_as_mut(&self) -> TokenStream {
        let snake_case = &self.snake_case;
        let data_type = &self.data_type.2;
        let pattern = &self.pattern;
        let construction = &self.construction.2;

        let fn_ident = format_ident!("{snake_case}_as_mut");

        quote! {
            fn #fn_ident(&mut self) -> Option< #data_type > {
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

    pub fn build_is_and(&self) -> TokenStream {
        let snake_case = &self.snake_case;
        let pattern = &self.pattern;

        let fn_ident = format_ident!("is_{snake_case}_and");

        quote! {
            fn #fn_ident(&self, f: impl FnOnce(&Self) -> bool) -> bool {
                match self {
                    #pattern => f(self),
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
                    _ => panic!("{}", msg),
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
