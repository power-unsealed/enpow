use helper::{ExtractEnumInfo, ExtractVariantInfo};
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, DeriveInput, Error};

mod helper;

#[proc_macro_derive(UnwrapAs)]
pub fn unwrap_as_entry(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match unwrap_as(input) {
        Ok(stream) => stream,
        Err(error) => error.into_compile_error(),
    }
    .into()
}

fn unwrap_as(input: DeriveInput) -> Result<TokenStream, Error> {
    let parent = input.extract_info()?;

    let variants: Vec<_> = parent
        .data
        .variants
        .iter()
        .cloned()
        .map(|var| var.extract_info(&parent))
        .collect::<Result<_, _>>()?;

    let mut functions = Vec::new();
    let mut type_defs = Vec::new();

    for variant in variants {
        functions.push(variant.build_base());
        functions.push(variant.build_base_as_ref());
        functions.push(variant.build_is());
        functions.push(variant.build_unwrap(&parent));
        functions.push(variant.build_unwrap_or());
        functions.push(variant.build_unwrap_or_else());
        functions.push(variant.build_expect());

        type_defs.extend(variant.type_defs.clone());
    }

    let enum_ident = &parent.identifier;
    let (gen_full, gen_short, gen_where) = parent.generics.split_for_impl();

    Ok(quote! {
        #(#[allow(unused)] #type_defs)*

        #[automatically_derived]
        #[allow(unused)]
        impl #gen_full #enum_ident #gen_short #gen_where {
            #(#functions)*
        }
    })
}

#[cfg(test)]
mod tests {
    use proc_macro2::TokenStream;
    use std::str::FromStr;
    use syn::{DeriveInput, Macro};

    #[test]
    fn unwrap_as_wrong_target() {
        let source = "struct A;";
        let tokens = TokenStream::from_str(source).unwrap();
        let input: DeriveInput = syn::parse2(tokens).unwrap();
        let result = super::unwrap_as(input).unwrap();
        let mcr: Macro = syn::parse2(result).unwrap();
        assert_eq!(mcr.path.get_ident().unwrap(), "compile_error");
    }

    #[test]
    fn unwrap_as() {
        let source =
            "#[derive(UnwrapAs)] pub enum Test<T> where T: Display { A, B(u32), C(i32, i64), D { a: u32, b: T }, }";
        let tokens = TokenStream::from_str(source).unwrap();
        let input: DeriveInput = syn::parse2(tokens).unwrap();
        let result = super::unwrap_as(input).unwrap();

        println!("{result}");
    }
}
