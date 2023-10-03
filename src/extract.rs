use std::collections::HashSet;

use crate::helper::{EnumInfoAdapter, VariantInfoAdapter, VariantType};
use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    Data, DeriveInput, Error, Fields, Ident, Token,
};

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

    // Get a reference to the output enum's data
    let output_data = match &mut output.data {
        Data::Enum(data) => data,
        _ => unreachable!("At that point it should be ensured that the macro target is an enum"),
    };

    let mut items = Vec::new();
    for (i, mut variant) in variants.into_iter().enumerate() {
        if types.contains(&variant.var_type.get_extract_type()) {
            // For each variant, build the data type and type definition
            let data_type = variant.build_extracted_self_type();
            let type_def = variant.build_self_type_def();

            // Manipulate the corresponding enum variant to contain only this new type
            output_data.variants[i].fields = Fields::Unnamed(syn::parse2(quote! { (#data_type) })?);

            // Generate the From trait for this extracted variant
            let from_impl = variant.build_from_impl_after_extraction(&parent);

            // Get the variant derives
            let derives = &variant.derives;
            let derives = quote! { #[derive(#(#derives),*)] };

            // Remove the original inner attributes from the ast if there are no other macro calls
            // that need them
            if parent.other_calls_from.is_none() {
                for (attr_removed, (idx, _)) in variant.inners.iter().enumerate() {
                    output_data.variants[i].attrs.remove(idx - attr_removed);
                }
            }

            // Save the type definition and implementation
            items.push(quote! {
                #derives
                #type_def
                #from_impl
            });
        }
    }

    // Remove each inner attribute without another macro call before it
    let remove_until = parent.other_calls_from.unwrap_or(parent.attributes.len());
    let mut attr_removed = 0;
    for (i, _) in parent.inners.iter() {
        if *i < remove_until {
            output.attrs.remove(i - attr_removed);
            attr_removed += 1;
        }
    }

    Ok(quote! {
        #output

        #(#items)*
    })
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExtractType {
    Unit,
    Single,
    Unnamed,
    Named,
}

impl ExtractType {
    pub fn from_attribute(attribute: TokenStream) -> Result<HashSet<ExtractType>, Error> {
        let info: ExtractAttributeInfo = syn::parse2(attribute)?;
        Ok(info.types)
    }
}

pub struct ExtractAttributeInfo {
    types: HashSet<ExtractType>,
}

impl Parse for ExtractAttributeInfo {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let mut items: Punctuated<_, Token![,]> = input.parse_terminated(Ident::parse)?;
        let mut types = HashSet::new();

        // If there are no arguments, insert a fake "All"
        if items.is_empty() {
            items.push(Ident::new("All", Span::call_site()));
        }

        for item in items {
            match item.to_string().as_str() {
                "All" => {
                    types.insert(ExtractType::Unit);
                    types.insert(ExtractType::Single);
                    types.insert(ExtractType::Unnamed);
                    types.insert(ExtractType::Named);
                }
                "Unit" => {
                    types.insert(ExtractType::Unit);
                }
                "Single" => {
                    types.insert(ExtractType::Single);
                }
                "Unnamed" => {
                    types.insert(ExtractType::Unnamed);
                }
                "Named" => {
                    types.insert(ExtractType::Named);
                }
                some => {
                    return Err(Error::new_spanned(
                        item,
                        format!("Unknown argument `{some}`"),
                    ));
                }
            }
        }

        Ok(ExtractAttributeInfo { types })
    }
}

impl VariantType {
    pub fn get_extract_type(&self) -> ExtractType {
        match self {
            VariantType::Unit => ExtractType::Unit,
            VariantType::Single(_) => ExtractType::Single,
            VariantType::Unnamed(_) => ExtractType::Unnamed,
            VariantType::Named(_) => ExtractType::Named,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::extract::ExtractType;
    use quote::quote;
    use std::collections::HashSet;

    #[test]
    fn extract_wrong_target() {
        let input = quote! { struct A; };

        let mut types = HashSet::new();
        types.insert(ExtractType::Unit);

        let result = super::generate(input, types);
        assert!(result.is_err());
    }

    #[test]
    fn extract() {
        let input = quote! {
            #[inner(derive(Clone, Debug, PartialEq))]
            enum IpAddress {
                None,
                #[inner(type_name="IpV4", derive(Copy))]
                V4(u8, u8, u8, u8),
                #[inner(type_name="IpV6")]
                V6(String),
                Multi {
                    v4: (u8, u8, u8, u8),
                    v6: String,
                },
            }
        };

        let mut types = HashSet::new();
        types.insert(ExtractType::Unit);
        types.insert(ExtractType::Single);
        types.insert(ExtractType::Unnamed);
        types.insert(ExtractType::Named);

        let result = super::generate(input, types).unwrap();

        println!("{result}");
    }
}
