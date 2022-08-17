use helper::{ExtractEnumInfo, ExtractVariantInfo};
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, DeriveInput};

mod helper;

#[proc_macro_derive(UnwrapAs)]
pub fn unwrap_as_entry(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    unwrap_as(input).into()
}

fn unwrap_as(input: DeriveInput) -> TokenStream {
    let parent = match input.extract_info() {
        Ok(enum_info) => enum_info,
        Err(error) => return error.to_compile_error(),
    };

    let variants: Vec<_> = parent
        .data
        .variants
        .iter()
        .map(|var| var.clone().extract_info(&parent))
        .collect();

    let mut functions = Vec::new();
    let mut type_defs = Vec::new();

    for variant in variants {
        functions.push(variant.build_base());
        functions.push(variant.build_is());
        functions.push(variant.build_unwrap(&parent));
        functions.push(variant.build_expect());

        type_defs.extend(variant.type_def.clone());
    }

    let enum_ident = &parent.identifier;
    let (gen_full, gen_short, gen_where) = parent.generics.split_for_impl();

    quote! {
        #(#[allow(unused)] #type_defs)*

        #[automatically_derived]
        #[allow(unused)]
        impl #gen_full #enum_ident #gen_short #gen_where {
            #(#functions)*
        }
    }
    .into()
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
        let result = super::unwrap_as(input);
        let mcr: Macro = syn::parse2(result).unwrap();
        assert_eq!(mcr.path.get_ident().unwrap(), "compile_error");
    }

    #[test]
    fn unwrap_as() {
        let source =
            "#[derive(UnwrapAs)] pub enum Test<T> where T: Display { A, B(u32), C(i32, i64), D { a: u32, b: T }, }";
        let tokens = TokenStream::from_str(source).unwrap();
        let input: DeriveInput = syn::parse2(tokens).unwrap();
        let result = super::unwrap_as(input);

        println!("{result}");
    }
}
