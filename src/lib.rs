use proc_macro2::{TokenStream, Span};
use quote::quote;
use syn::{parse_macro_input, Ident, DeriveInput, Data, Fields, spanned::Spanned, Error};

mod helper;

#[proc_macro_derive(UnwrapAs)]
pub fn unwrap_as_entry(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    unwrap_as(input).into()
}

fn unwrap_as(input: DeriveInput) -> TokenStream {
    let span = input.span();
    let visibility = input.vis;
    let ident = input.ident;
    let name = ident.to_string();
    let data = match input.data {
        Data::Enum(data) => data,
        _ => {
            return Error::new(span, "`UnwrapAs` can only be derived for enums")
                .to_compile_error();
        }
    };

    let mut functions = Vec::new();
    let mut types = Vec::new();
    for variant in data.variants {
        let var_ident = variant.ident;
        let var_name = var_ident.to_string();

        // Build function identifier with prefix and variant name in snake_case
        let sc_ident = helper::to_snake_case(&var_name);
        let unwrap_as_name = format!("unwrap_as_{sc_ident}");
        let unwrap_as_ident = Ident::new(&unwrap_as_name, Span::call_site());

        // Build return type definition, match pattern, and instance construction code
        let (type_ident, type_def, pattern, construction) = match variant.fields {
            // Variant without data
            Fields::Unit => (
                quote! { () },
                quote! { },
                quote! { #ident::#var_ident },
                quote! { () },
            ),
            // Variant with exactly one unnamed field
            Fields::Unnamed(tuple) if tuple.unnamed.len() == 1 => {
                let single = &tuple.unnamed[0].ty;
                (
                    quote! { #single },
                    quote! { },
                    quote! { #ident::#var_ident( f0 ) },
                    quote! { f0 },
                )
            },
            // Variant with more than one unnamed field
            Fields::Unnamed(tuple) => {
                let fields: Vec<_> = tuple.unnamed.iter().enumerate()
                    .map(|(i, _)| {
                        let name = format!("f{i}");
                        Ident::new(&name, Span::call_site())
                    })
                    .collect();

                (
                    quote! { #tuple },
                    quote! { },
                    quote! { #ident::#var_ident( #(#fields),* ) },
                    quote! { ( #(#fields),* ) },
                )
            },
            // Variant with named fields
            Fields::Named(fields) => {
                // Build return type identifier with prefix, enum name, and variant name
                // in CamelCase
                let type_name = format!("UnwrapAs{name}{var_name}");
                let type_ident = Ident::new(&type_name, Span::call_site());

                let (field_idents, field_types): (Vec<_>, Vec<_>) = fields.named.into_iter()
                    .map(|field| {
                        (field.ident.unwrap(), field.ty)
                    })
                    .unzip();
                let fields: Vec<_> = field_idents.iter().zip(field_types.iter())
                    .map(|(i, t)| quote! { pub #i: #t })
                    .collect();
                
                (
                    quote! { #type_ident },
                    quote! { #visibility struct #type_ident { #(#fields),* } },
                    quote! { #ident::#var_ident { #(#field_idents),* } },
                    quote! { #type_ident { #(#field_idents),* } },
                )
            },
        };

        let panic_msg = format!(
            "Failed unwrapping to {name}::{var_name}. Unexpected variant"
        );

        let function = quote! {
            fn #unwrap_as_ident(self) -> #type_ident {
                match self {
                    #pattern => #construction,
                    _ => panic!(#panic_msg),
                }
            }
        };

        functions.push(function);
        if !type_def.is_empty() {
            types.push(type_def);
        }
    }

    quote! {
        #(#[allow(dead_code)] #types)*

        impl #ident {
            #(#[allow(dead_code)] #functions)*
        }
    }.into()
}

#[cfg(test)]
mod tests {
    use proc_macro2::TokenStream;
    use syn::{DeriveInput, Macro};
    use std::str::FromStr;

    #[test]
    fn unwrap_as_wrong_target() {
        let source = "struct A;";
        let tokens = TokenStream::from_str(source).unwrap();
        let input: DeriveInput = syn::parse2(tokens).unwrap();
        let result = super::unwrap_as(input);
        let mcr: Macro = syn::parse2(result).unwrap();
        assert_eq!(mcr.path.get_ident().unwrap(), "compile_error");
    }

    #[test]
    fn unwrap_as() {
        let source = "enum Test { A, B(u32), C(u32, u32), D { a: u32, b: u32 } }";
        let tokens = TokenStream::from_str(source).unwrap();
        let input: DeriveInput = syn::parse2(tokens).unwrap();
        let result = super::unwrap_as(input);
        println!("{result}");
    }
}
