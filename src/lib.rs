use helper::{ExtractEnumInfo, ExtractVariantInfo, MethodType};
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

    let generated = generate(item.clone(), &types)?;

    Ok(quote! {
        #item
        #generated
    })
}

fn generate(input: TokenStream, types: &[MethodType]) -> Result<TokenStream, Error> {
    // Find out which type definition are necessary
    let (mut gen_self_def, mut gen_ref_def, mut gen_mut_def) = (false, false, false);
    for t in types {
        gen_self_def = gen_self_def || t.needs_self_type();
        gen_ref_def = gen_ref_def || t.needs_ref_type();
        gen_mut_def = gen_mut_def || t.needs_mut_type();
    }

    let input: DeriveInput = syn::parse2(input)?;
    let parent = input.extract_info()?;

    let variants: Vec<_> = parent
        .data
        .variants
        .iter()
        .cloned()
        .map(|var| var.extract_info(&parent))
        .collect::<Result<_, _>>()?;

    let mut functions = Vec::new();
    let mut self_defs = Vec::new();
    let mut ref_defs = Vec::new();
    let mut mut_defs = Vec::new();

    for variant in variants {
        functions.extend(variant.build_method_types(&parent, &types));

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

    let enum_ident = &parent.identifier;
    let visibility = &parent.visibility;
    let (gen_full, gen_short, gen_where) = parent.generics.split_for_impl();

    Ok(quote! {
        #(#[allow(unused)] #self_defs)*
        #(#[allow(unused)] #[derive(Clone, Copy)] #ref_defs)*
        #(#[allow(unused)] #mut_defs)*

        #[automatically_derived]
        #[allow(unused)]
        impl #gen_full #enum_ident #gen_short #gen_where {
            #(#visibility #functions)*
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
        let result = super::generate(input, &[]).unwrap();
        let mcr: Macro = syn::parse2(result).unwrap();
        assert_eq!(mcr.path.get_ident().unwrap(), "compile_error");
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
