use crate::helper::{
    ExtractEnumInfo, ExtractVariantInfo, GenericsFilter, VarDeriveAttributeInfo, VariantType,
};
use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote};
use syn::{Data, DeriveInput, Error, Fields, ItemStruct, TypeTuple};

pub fn entry(_attribute: TokenStream, item: TokenStream) -> Result<TokenStream, Error> {
    generate(item)
}

fn generate(input: TokenStream) -> Result<TokenStream, Error> {
    // Extract the necessray information from the given token stream
    let input: DeriveInput = syn::parse2(input)?;
    let mut output = input.clone();
    let parent = input.extract_info()?;
    let variants: Vec<_> = parent
        .data
        .variants
        .iter()
        .cloned()
        .map(|var| var.extract_info(&parent))
        .collect::<Result<_, _>>()?;

    let enum_ident = &parent.identifier;
    let visibility = &parent.visibility;
    let generics = &parent.generics;
    let mut type_defs = Vec::new();
    for (i, variant) in variants.into_iter().enumerate() {
        // For each variant, build the data type and type definition
        let identifier = &variant.identifier;
        let type_ident = format_ident!("{enum_ident}{identifier}");
        let (data_type, type_def) = match variant.var_type {
            VariantType::Unit => (
                quote! { #type_ident },
                quote! { #visibility struct #type_ident; },
            ),
            VariantType::Field => {
                let single = variant.data_type.0;

                // Filter unused generics
                let without_gen = quote! {
                    #visibility struct #type_ident ( pub #single );
                };
                let ast = syn::parse2::<ItemStruct>(without_gen)?;
                let generics = generics.filter_unused(&ast)?;
                let (gen_main, gen_short, gen_where) = generics.split_for_impl();

                (
                    quote! { #type_ident #gen_short },
                    quote! { #visibility struct #type_ident #gen_main ( pub #single ) #gen_where; },
                )
            }
            VariantType::Unnamed => {
                let tuple: TypeTuple = syn::parse2(variant.data_type.0)?;
                let fields: Vec<_> = tuple.elems.into_iter().collect();

                // Filter unused generics
                let without_gen = quote! {
                    #visibility struct #type_ident ( #(pub #fields),* );
                };
                let ast = syn::parse2::<ItemStruct>(without_gen)?;
                let generics = generics.filter_unused(&ast)?;
                let (gen_main, gen_short, gen_where) = generics.split_for_impl();

                (
                    quote! { #type_ident #gen_short },
                    quote! {
                        #visibility struct #type_ident #gen_main ( #(pub #fields),* ) #gen_where;
                    },
                )
            }
            VariantType::Named => (variant.data_type.0, variant.type_def.0.unwrap()),
        };

        // Manipulate the corresponding enum variant to contain only this new type
        let data = match &mut output.data {
            Data::Enum(data) => data,
            _ => {
                return Err(Error::new(
                    Span::call_site(),
                    "Can only be derived for enums",
                ));
            }
        };
        data.variants[i].fields = Fields::Unnamed(syn::parse2(quote! { (#data_type) })?);

        // Save the type definition
        type_defs.push(type_def);
    }

    // Check for every attached attribute `extract_derive`
    let mut derives = Vec::new();
    let mut attr_removed = 0;
    for (i, attr) in parent.attributes.iter().enumerate() {
        let ident = attr.path.get_ident();
        if ident
            .map(|i| i.to_string() == "extract_derive")
            .unwrap_or(false)
        {
            // Get the derive Traits
            let info: VarDeriveAttributeInfo = syn::parse2(attr.tokens.clone())?;
            derives.extend(info.derives);

            // Remove this attribute from the output ast
            output.attrs.remove(i - attr_removed);
            attr_removed += 1;
        }
    }

    // Build the derive tokens
    let derives = quote! { #[derive(#(#derives),*)] };

    Ok(quote! {
        #output

        #(#derives #type_defs)*
    })
}

#[cfg(test)]
mod tests {
    use proc_macro2::TokenStream;
    use std::str::FromStr;

    #[test]
    fn extract_wrong_target() {
        let source = "struct A;";
        let input = TokenStream::from_str(source).unwrap();
        let result = super::generate(input);
        assert!(result.is_err());
    }

    #[test]
    fn extract() {
        let source = "
        #[extract_derive(Debug, PartialEq)]
        #[derive(Debug, PartialEq)]
        pub enum Inner<T, S: ToString> {
            A,
            B(T),
            C(T, S),
            D { a: T, b: S },
        }";
        let input = TokenStream::from_str(source).unwrap();
        let result = super::generate(input).unwrap();

        println!("{result}");
    }
}
