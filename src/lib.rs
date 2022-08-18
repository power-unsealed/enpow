use helper::{ExtractEnumInfo, ExtractVariantInfo, MethodType, VarDeriveAttributeInfo};
use proc_macro2::TokenStream;
use quote::quote;
use syn::{DeriveInput, Error};

mod helper;

#[proc_macro_attribute]
pub fn enpow(
    attribute: proc_macro::TokenStream,
    item: proc_macro::TokenStream
) -> proc_macro::TokenStream {
    match enpow2(attribute.into(), item.into()) {
        Ok(stream) => stream,
        Err(error) => error.to_compile_error(),
    }
    .into()
}

fn enpow2(attribute: TokenStream, item: TokenStream) -> Result<TokenStream, Error> {
    let types: Vec<_> = MethodType::from_attribute(attribute)?
        .into_iter()
        .collect();

    generate(item, &types)
}

fn generate(input: TokenStream, types: &[MethodType]) -> Result<TokenStream, Error> {
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

    // Generate all requested methods
    let mut methods = Vec::new();
    let mut self_defs = Vec::new();
    let mut ref_defs = Vec::new();
    let mut mut_defs = Vec::new();
    for variant in variants {
        methods.extend(variant.build_method_types(&parent, &types));

        // Save type definitions as needed
        if gen_self_def {
            self_defs.extend(variant.type_def.0.clone());
        }
        if gen_ref_def {
            ref_defs.extend(variant.type_def.1.clone());
        }
        if gen_mut_def {
            mut_defs.extend(variant.type_def.2.clone());
        }
    }

    // Check for every attached attribute `var_derive`
    let mut derives = Vec::new();
    let mut attr_removed = 0;
    for (i, attr) in parent.attributes.iter().enumerate() {
        let ident = attr.path.get_ident();
        if ident.map(|i| i.to_string() == "var_derive").unwrap_or(false) {
            // Get the derive Traits
            let info: VarDeriveAttributeInfo = syn::parse2(attr.tokens.clone())?;
            derives.extend(info.derives);

            // Remove this attribute from the output ast
            output.attrs.remove(i - attr_removed);
            attr_removed += 1;
        }
    }
    let derives = quote! { #[derive(#(#derives),*)] };

    let enum_ident = &parent.identifier;
    let visibility = &parent.visibility;
    let (gen_full, gen_short, gen_where) = parent.generics.split_for_impl();

    Ok(quote! {
        #output

        #(#[allow(unused)] #derives #self_defs)*
        #(#[allow(unused)] #[derive(Clone, Copy)] #ref_defs)*
        #(#[allow(unused)] #mut_defs)*

        #[automatically_derived]
        #[allow(unused)]
        impl #gen_full #enum_ident #gen_short #gen_where {
            #(#visibility #methods)*
        }
    })
}

#[cfg(test)]
mod tests {
    use proc_macro2::TokenStream;
    use std::str::FromStr;
    use syn::Macro;
    use crate::helper::MethodType;

    #[test]
    fn unwrap_as_wrong_target() {
        let source = "struct A;";
        let input = TokenStream::from_str(source).unwrap();
        let result = super::generate(input, &[]);
        assert!(result.is_err());
    }

    #[test]
    fn unwrap_as() {
        let source =
            "#[derive(UnwrapAs)] pub enum Test<T> where T: Display { A, B(u32), C(i32, i64), D { a: u32, b: T }, }";
        let input = TokenStream::from_str(source).unwrap();
        let result = super::generate(input, &[MethodType::All]).unwrap();

        println!("{result}");
    }
}
