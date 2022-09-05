use std::collections::HashSet;

use crate::helper::{
    DeriveAttributeInfo, EnumInfo, EnumInfoAdapter, VariantInfo, VariantInfoAdapter, VariantType,
};
use proc_macro2::{Span, TokenStream};
use quote::{quote, quote_spanned, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    DeriveInput, Error, Ident, Token,
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

    // Check all variants whether there is a name clash when turned into snake case
    let var_names: Vec<_> = variants
        .iter()
        .map(|v| (v.snake_case.clone(), v.identifier.span()))
        .collect();
    for (i, (i_name, _)) in var_names.iter().enumerate() {
        for (j, (j_name, span)) in var_names.iter().enumerate() {
            if i != j && i_name == j_name {
                let i_ident = &variants[i].identifier;
                return Err(Error::new(
                    span.clone(),
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
    for mut variant in variants {
        // Build all selected method types for that variant
        methods.extend(variant.build_method_types(&parent, &types));

        // Build type definitions as needed
        if let VariantType::Named(_) = &variant.var_type {
            if gen_self_def {
                self_defs.push(variant.build_self_type_def());
            }
            if gen_ref_def {
                ref_defs.push(variant.build_ref_type_def());
            }
            if gen_mut_def {
                mut_defs.push(variant.build_mut_type_def());
            }
        }
    }

    // DEPRECATED
    // Check for every attached attribute `enpow_derive`
    let mut self_derives = Vec::new();
    let mut attr_removed = 0;
    for (i, attr) in parent.attributes.iter().enumerate() {
        let ident = attr.path.get_ident();
        if ident
            .map(|i| i.to_string() == "enpow_derive")
            .unwrap_or(false)
        {
            // Get the derive Traits
            let info: DeriveAttributeInfo = syn::parse2(attr.tokens.clone())?;
            self_derives.extend(info.derives);

            // Remove this attribute from the output ast
            output.attrs.remove(i - attr_removed);
            attr_removed += 1;
        }
    }

    // Get the derives from the inner attributes and remove the original inner attributes from the
    // ast
    self_derives.extend(parent.derives.clone());
    for (i, _) in parent.inners.iter() {
        output.attrs.remove(i - attr_removed);
        attr_removed += 1;
    }

    // Ref already has Clone and Copy, while mut is not allowed to get Clone and Copy.
    // Because of the dynamic import system, we cannot say for sure how these macros are
    // identified -> best efford
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

    // Build the derive macro tokens
    let self_derives = quote! { #[derive(#(#self_derives),*)] };
    let ref_derives = quote! { #[derive(#(#ref_derives),*)] };

    let enum_ident = &parent.identifier;
    let (gen_full, gen_short, gen_where) = parent.generics.split_for_impl();

    // Generate the output tokens, while preserving the span for the original enum, but pointing
    // all additional tokens to the call site
    let original_tokens = output.to_token_stream();
    let additional_tokens = quote_spanned! { Span::call_site() =>
        #(#[allow(unused)] #self_derives #self_defs)*
        #(#[allow(unused)] #ref_derives #[derive(Clone, Copy)] #ref_defs)*
        #(#[allow(unused)] #ref_derives #mut_defs)*

        #[automatically_derived]
        #[allow(unused)]
        impl #gen_full #enum_ident #gen_short #gen_where {
            #(#methods)*
        }
    };

    Ok(quote! {
        #original_tokens
        #additional_tokens
    })
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum EnpowType {
    Variant,
    IsVariant,
    VariantAsRef,
    UnwrapVariant,
    ExpectVariant,
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
            EnpowType::UnwrapVariant => true,
            EnpowType::ExpectVariant => true,
        }
    }

    pub fn needs_ref_type(&self) -> bool {
        match self {
            EnpowType::Variant => false,
            EnpowType::IsVariant => true,
            EnpowType::VariantAsRef => true,
            EnpowType::UnwrapVariant => true,
            EnpowType::ExpectVariant => true,
        }
    }

    pub fn needs_mut_type(&self) -> bool {
        match self {
            EnpowType::Variant => false,
            EnpowType::IsVariant => false,
            EnpowType::VariantAsRef => true,
            EnpowType::UnwrapVariant => true,
            EnpowType::ExpectVariant => true,
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
                EnpowType::UnwrapVariant => {
                    methods.push(self.build_unwrap(parent));
                    methods.push(self.build_unwrap_as_ref(parent));
                    methods.push(self.build_unwrap_as_mut(parent));
                    methods.push(self.build_unwrap_or());
                    methods.push(self.build_unwrap_or_else());
                }
                EnpowType::ExpectVariant => {
                    methods.push(self.build_expect());
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
            #[enpow_derive(Debug, PartialEq)]
            #[inner(derive(Clone))]
            #[derive(Clone, Debug, PartialEq)]
            pub enum Token<'a, Span> {
                /// `+`
                Plus(
                    /// Source span
                    &'a Span
                ),
                /// Unsigned integer literal
                Number {
                    /// Source span
                    span: &'a Span,
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
