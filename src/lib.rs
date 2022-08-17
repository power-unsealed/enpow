use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, Data, spanned::Spanned, Error};

#[proc_macro_derive(UnwrapAs)]
pub fn unwrap_as_entry(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    unwrap_as(input).into()
}

fn unwrap_as(input: DeriveInput) -> TokenStream {
    let span = input.span();
    let name = input.ident;
    let data = match input.data {
        Data::Enum(data) => data,
        _ => {
            return Error::new(span, "`UnwrapAs` can only be derived for enums")
                .to_compile_error();
        }
    };

    for variant in data.variants {
        //variant.
    }

    quote! {
        
    }.into()
}

#[cfg(test)]
mod tests {
    use super::*;
    use syn::Macro;
    use std::str::FromStr;

    #[test]
    fn unwrap_as_wrong_target() {
        let source = "#[derive(UnwrapAs)] struct A;";
        let tokens = TokenStream::from_str(source).unwrap();
        let input: DeriveInput = syn::parse2(tokens).unwrap();
        let result = unwrap_as(input);
        let mcr: Macro = syn::parse2(result).unwrap();
        assert_eq!(mcr.path.get_ident().unwrap(), "compile_error");
    }
}
