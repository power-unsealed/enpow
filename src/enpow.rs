use std::collections::HashSet;

use crate::helper::{
    EnumInfo, EnumInfoAdapter, VariantInfo, VariantInfoAdapter, VariantType,
};
use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    DeriveInput, Error, Ident, Token, Data,
};

pub fn entry(attribute: TokenStream, item: TokenStream) -> Result<TokenStream, Error> {
    let types: Vec<_> = EnpowType::from_attribute(attribute)?.into_iter().collect();

    generate(item, &types)
}

fn generate(input: TokenStream, types: &[EnpowType]) -> Result<TokenStream, Error> {
    // Find out which type definition are necessary
    let mut gen_self_def = false;
    let mut gen_ref_def = false;
    let mut gen_mut_def = false;
    for t in types {
        gen_self_def = gen_self_def || t.needs_self_type();
        gen_ref_def = gen_ref_def || t.needs_ref_type();
        gen_mut_def = gen_mut_def || t.needs_mut_type();
    }

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

    // Check all variants whether there is a name clash when turned into snake case
    let var_names: Vec<_> = variants
        .iter()
        .map(|v| (v.method_name.clone(), v.identifier.span()))
        .collect();
    for (i, (i_name, i_span)) in var_names.iter().enumerate() {
        for (j, (j_name, j_span)) in var_names.iter().enumerate() {
            if i != j && i_name == j_name {
                let i_ident = &variants[i].identifier;
                Error::new(i_span.clone(), "Conflicting variant defined here")
                    .into_compile_error();
                return Err(Error::new(
                    j_span.clone(),
                    format!(
                        "Identifier clashes with other variant `{i_ident}` when turned into \
                        snake case: `{i_name}`. Consider renaming one of the variants"
                    ),
                ));
            }
        }
    }

    // Generate all requested methods
    let mut methods = Vec::new();
    let mut self_defs = Vec::new();
    let mut ref_defs = Vec::new();
    let mut mut_defs = Vec::new();
    let mut from_impls = Vec::new();
    for (i, mut variant) in variants.into_iter().enumerate() {
        // Build all selected method types for that variant
        methods.extend(variant.build_method_types(&parent, &types));

        // Get the variant derives
        let self_derives = variant.derives.clone();

        // Ref will always get Clone and Copy, while mut is not allowed to get Clone and Copy.
        // Because of the dynamic import system, we cannot say for sure how these macros are
        // identified -> best efford filter
        let ref_derives: Vec<_> = self_derives
            .iter()
            .filter_map(|path| {
                let last = path.segments.last().map(|l| l.to_token_stream().to_string());
                if let Some("Copy") | Some("Clone") = last.as_ref().map(|s| s.as_str()) {
                    None
                } else {
                    Some(path)
                }
            })
            .collect();

        // Build type definitions as needed
        if let VariantType::Named(_) = &variant.var_type {
            if gen_self_def {
                // For self types we also add an automatic from implementation
                let self_def = variant.build_self_type_def();
                self_defs.push(quote! {
                    #[derive( #(#self_derives),* )]
                    #self_def
                });
                
                // Build the from implementations
                from_impls.push(variant.build_from_impl_without_extraction(&parent));
            }

            if gen_ref_def {
                let ref_def = variant.build_ref_type_def();
                ref_defs.push(quote! {
                    #[derive( Clone, Copy, #(#ref_derives),* )]
                    #ref_def
                });
            }

            if gen_mut_def {
                let mut_def = variant.build_mut_type_def();
                mut_defs.push(quote! {
                    #[derive( #(#ref_derives),* )]
                    #mut_def
                });
            }
        }
        
        // Remove the original inner attributes from the ast if there are no other macro calls
        // that need them
        if parent.other_calls_from.is_none() {
            for (attr_removed, (idx, _)) in variant.inners.iter().enumerate() {
                output_data.variants[i].attrs.remove(idx - attr_removed);
            }
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

    let enum_ident = &parent.identifier;
    let (gen_full, gen_short, gen_where) = parent.generics.split_for_impl();

    Ok(quote! {
        #output

        #(#from_impls)*

        #(#[allow(unused)] #self_defs)*
        #(#[allow(unused)] #ref_defs)*
        #(#[allow(unused)] #mut_defs)*

        #[automatically_derived]
        #[allow(unused)]
        impl #gen_full #enum_ident #gen_short #gen_where {
            #(#methods)*
        }
    })
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum EnpowType {
    Variant,
    IsVariant,
    VariantAsRef,
    MapVariant,
    UnwrapVariant,
    UnwrapVariantAsRef,
    ExpectVariant,
    ExpectVariantAsRef,
}

impl EnpowType {
    pub fn from_attribute(attribute: TokenStream) -> Result<HashSet<EnpowType>, Error> {
        let info: EnpowAttributeInfo = syn::parse2(attribute)?;
        Ok(info.methods)
    }

    pub fn needs_self_type(&self) -> bool {
        match self {
            EnpowType::Variant => true,
            EnpowType::IsVariant => false,
            EnpowType::VariantAsRef => false,
            EnpowType::MapVariant => true,
            EnpowType::UnwrapVariant => true,
            EnpowType::UnwrapVariantAsRef => false,
            EnpowType::ExpectVariant => true,
            EnpowType::ExpectVariantAsRef => false,
        }
    }

    pub fn needs_ref_type(&self) -> bool {
        match self {
            EnpowType::Variant => false,
            EnpowType::IsVariant => true,
            EnpowType::VariantAsRef => true,
            EnpowType::MapVariant => false,
            EnpowType::UnwrapVariant => false,
            EnpowType::UnwrapVariantAsRef => true,
            EnpowType::ExpectVariant => false,
            EnpowType::ExpectVariantAsRef => true,
        }
    }

    pub fn needs_mut_type(&self) -> bool {
        match self {
            EnpowType::Variant => false,
            EnpowType::IsVariant => false,
            EnpowType::VariantAsRef => true,
            EnpowType::MapVariant => false,
            EnpowType::UnwrapVariant => false,
            EnpowType::UnwrapVariantAsRef => true,
            EnpowType::ExpectVariant => false,
            EnpowType::ExpectVariantAsRef => true,
        }
    }
}

pub struct EnpowAttributeInfo {
    methods: HashSet<EnpowType>,
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
                    methods.insert(EnpowType::Variant);
                    methods.insert(EnpowType::IsVariant);
                    methods.insert(EnpowType::VariantAsRef);
                    methods.insert(EnpowType::MapVariant);
                    methods.insert(EnpowType::UnwrapVariant);
                    methods.insert(EnpowType::ExpectVariant);
                }
                "Var" => {
                    methods.insert(EnpowType::Variant);
                }
                "IsVar" => {
                    methods.insert(EnpowType::IsVariant);
                }
                "VarAsRef" => {
                    methods.insert(EnpowType::VariantAsRef);
                }
                "MapVar" => {
                    methods.insert(EnpowType::MapVariant);
                }
                "UnwrapVar" => {
                    methods.insert(EnpowType::UnwrapVariant);
                }
                "ExpectVar" => {
                    methods.insert(EnpowType::ExpectVariant);
                }
                some => {
                    return Err(Error::new_spanned(
                        item,
                        format!("Unknown argument `{some}`"),
                    ));
                }
            }
        }

        // If there are both (`UnwrapVar` OR `ExpectVar`) AND `VarAsRef`, generate the special
        // methods
        if methods.contains(&EnpowType::VariantAsRef) {
            if methods.contains(&EnpowType::UnwrapVariant) {
                methods.insert(EnpowType::UnwrapVariantAsRef);
            }

            if methods.contains(&EnpowType::ExpectVariant) {
                methods.insert(EnpowType::ExpectVariantAsRef);
            }
        }

        Ok(EnpowAttributeInfo { methods })
    }
}

impl VariantInfo {
    pub fn build_method_types(
        &mut self,
        parent: &EnumInfo,
        types: &[EnpowType],
    ) -> Vec<TokenStream> {
        let mut methods = Vec::new();
        for t in types {
            match t {
                EnpowType::Variant => {
                    methods.push(self.build_variant());
                }
                EnpowType::IsVariant => {
                    methods.push(self.build_is());
                    methods.push(self.build_is_and());
                }
                EnpowType::VariantAsRef => {
                    methods.push(self.build_as_ref());
                    methods.push(self.build_as_mut());
                }
                EnpowType::MapVariant => {
                    methods.push(self.build_map_or());
                    methods.push(self.build_map_or_else());
                }
                EnpowType::UnwrapVariant => {
                    methods.push(self.build_unwrap(parent));
                    methods.push(self.build_unwrap_or());
                    methods.push(self.build_unwrap_or_else());
                }
                EnpowType::UnwrapVariantAsRef => {
                    methods.push(self.build_unwrap_as_ref(parent));
                    methods.push(self.build_unwrap_as_mut(parent));
                }
                EnpowType::ExpectVariant => {
                    methods.push(self.build_expect());
                }
                EnpowType::ExpectVariantAsRef => {
                    methods.push(self.build_expect_as_ref());
                    methods.push(self.build_expect_as_mut());
                }
            }
        }
        methods
    }
}

#[cfg(test)]
mod tests {
    use crate::enpow::EnpowType;
    use quote::quote;

    #[test]
    fn enpow_wrong_target() {
        let input = quote! { struct A; };
        let result = super::generate(input, &[]);
        assert!(result.is_err());
    }

    #[test]
    fn enpow() {
        let input = quote! {
            #[inner(derive(Clone, Debug, PartialEq))]
            #[derive(Clone, Debug, PartialEq)]
            pub enum Token<Span> {
                /// `+`
                #[inner(type_name="Plus")]
                Plus(
                    /// Source span
                    Span
                ),
                /// Unsigned integer literal
                #[inner(type_name="Number", derive(Copy))]
                Number {
                    /// Source span
                    span: Span,
                    /// Value
                    value: u64,
                }
            }
        };

        let types = [EnpowType::Variant, EnpowType::VariantAsRef];
        let result = super::generate(input, &types).unwrap();

        println!("{result}");
    }
}
