use std::collections::HashSet;

use crate::helper::{ExtractEnumInfo, ExtractType, ExtractVariantInfo, VarDeriveAttributeInfo};
use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{Data, DeriveInput, Error, Fields};

pub fn entry(attribute: TokenStream, item: TokenStream) -> Result<TokenStream, Error> {
    let types = ExtractType::from_attribute(attribute)?;

    generate(item, types)
}

fn generate(input: TokenStream, types: HashSet<ExtractType>) -> Result<TokenStream, Error> {
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

    let mut type_defs = Vec::new();
    for (i, mut variant) in variants.into_iter().enumerate() {
        if types.contains(&variant.var_type.get_extract_type()) {
            // For each variant, build the data type and type definition
            let data_type = variant.build_extracted_self_type();
            let type_def = variant.build_self_type_def();
    
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
    use std::{str::FromStr, collections::HashSet};
    use crate::helper::ExtractType;

    #[test]
    fn extract_wrong_target() {
        let source = "struct A;";
        let input = TokenStream::from_str(source).unwrap();
        
        let mut types = HashSet::new();
        types.insert(ExtractType::Unit);

        let result = super::generate(input, types);
        assert!(result.is_err());
    }

    #[test]
    fn extract() {
        let source = "
        #[extract_derive(Debug, PartialEq)]
        #[derive(Debug, PartialEq)]
        pub enum Inner<T, S: ToString> {
            /// Doc for `A`
            A,
            /// Doc for `B`
            B(T),
            /// Doc for `C`
            C(T, S),
            /// Doc for `D`
            D {
                /// Doc for `D.a`
                a: T,
                /// Doc for `D.b`
                b: S
            },
        }";
        let input = TokenStream::from_str(source).unwrap();

        let mut types = HashSet::new();
        types.insert(ExtractType::Single);
        types.insert(ExtractType::Unnamed);
        types.insert(ExtractType::Named);

        let result = super::generate(input, types).unwrap();

        println!("{result}");
    }
}
