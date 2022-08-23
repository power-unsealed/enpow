//! EnPow is a procedural macro crate used to enpower user defined enums with many methods usually
//! known from the standard library's `Result<T, E>` and `Option<T>`. It can generate methods
//! like `fn is_<variant>(&self) -> bool` or `fn unwrap_<variant>(self) -> <inner>`, supporting
//! variants with named or unnamed fields, as well as generics. See the
//! [macro's documentation](macro@enpow) for details on the specific methods supported.
//! 
//! ## Usage Example
//! 
//! ```rust
//! use enpow::enpow;
//!
//! #[enpow(Var, VarAsRef)]
//! #[enpow_derive(Debug, PartialEq)]
//! #[derive(Clone, Debug, PartialEq)]
//! pub enum Token<Span> {
//!     Plus(Span),
//!     Minus(Span),
//!     Number {
//!         span: Span,
//!         value: u64,
//!     }
//! }
//! ```
//!
//! This example will evaluate to the following code.
//!
//! ```rust
//! use enpow::enpow;
//!
//! #[derive(Clone, Debug, PartialEq)]
//! pub enum Token<Span> {
//!     Plus(Span),
//!     Minus(Span),
//!     Number { span: Span, value: u64 },
//! }
//! 
//! #[allow(unused)]
//! #[derive(Debug, PartialEq)]
//! pub struct TokenNumber<Span> {
//!     pub span: Span,
//!     pub value: u64,
//! }
//! 
//! #[allow(unused)]
//! #[derive(Debug, PartialEq, Clone, Copy)]
//! pub struct TokenNumberRef<'token_number, Span> {
//!     pub span: &'token_number Span,
//!     pub value: &'token_number u64,
//! }
//! 
//! #[allow(unused)]
//! #[derive(Debug, PartialEq)]
//! pub struct TokenNumberMut<'token_number, Span> {
//!     pub span: &'token_number mut Span,
//!     pub value: &'token_number mut u64,
//! }
//! 
//! #[automatically_derived]
//! #[allow(unused)]
//! impl<Span> Token<Span> {
//!     pub fn plus(self) -> Option<Span> {
//!         match self {
//!             Token::Plus(f0) => Some(f0),
//!             _ => None,
//!         }
//!     }
//! 
//!     pub fn plus_as_ref(&self) -> Option<&Span> {
//!         match self {
//!             Token::Plus(f0) => Some(f0),
//!             _ => None,
//!         }
//!     }
//! 
//!     pub fn plus_as_mut(&mut self) -> Option<&mut Span> {
//!         match self {
//!             Token::Plus(f0) => Some(f0),
//!             _ => None,
//!         }
//!     }
//! 
//!     pub fn minus(self) -> Option<Span> {
//!         match self {
//!             Token::Minus(f0) => Some(f0),
//!             _ => None,
//!         }
//!     }
//! 
//!     pub fn minus_as_ref(&self) -> Option<&Span> {
//!         match self {
//!             Token::Minus(f0) => Some(f0),
//!             _ => None,
//!         }
//!     }
//! 
//!     pub fn minus_as_mut(&mut self) -> Option<&mut Span> {
//!         match self {
//!             Token::Minus(f0) => Some(f0),
//!             _ => None,
//!         }
//!     }
//! 
//!     pub fn number(self) -> Option<TokenNumber<Span>> {
//!         match self {
//!             Token::Number { span, value } => Some(TokenNumber { span, value }),
//!             _ => None,
//!         }
//!     }
//! 
//!     pub fn number_as_ref(&self) -> Option<TokenNumberRef<Span>> {
//!         match self {
//!             Token::Number { span, value } => Some(TokenNumberRef { span, value }),
//!             _ => None,
//!         }
//!     }
//! 
//!     pub fn number_as_mut(&mut self) -> Option<TokenNumberMut<Span>> {
//!         match self {
//!             Token::Number { span, value } => Some(TokenNumberMut { span, value }),
//!             _ => None,
//!         }
//!     }
//! }
//! 
//! assert_eq!(Token::Plus(3).plus(), Some(3));
//! assert_eq!(Token::Minus(7).plus(), None);
//! assert_eq!(Token::Number { span: 0, value: 42 }.number().unwrap().span, 0);
//! 
//! let mut num = Token::Number { span: 10, value: 7 };
//! *num.number_as_mut().unwrap().span = 20;
//! assert_eq!(num.number(), Some(TokenNumber { span: 20, value: 7 }))
//! ```

use helper::{ExtractEnumInfo, ExtractVariantInfo, MethodType, VarDeriveAttributeInfo};
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{DeriveInput, Error};

mod helper;

///////////////////////////////////////////////////////////////////////////////////////////////////
/// The `enpow` attribute attached to the target enum derives typical methods for working with
/// variants as known from `Result<T, E>` and `Option<T>`. It supports generics and variants of
/// every type, with named or unnamed fields or no fields attached. Variants with unnamed fields
/// get unwrapped into a tuple, while variants with named fields are transformed into an
/// automatically generated struct named after the enum and variant, i.e. `EnumVariant`. The
/// functions and struct generated inherit the visibility modifier of the target enum.
///
/// In parethesis, the following arguments to `enpow` can be used to specify which methods to
/// generate. Without any arguments, all methods will be generated. The method identifiers are
/// generated from the variant names turned into snake case.
///
/// - `Var`
///     * `fn <variant>(self) -> Option<<inner>>`
///     Return the inner data, if the enum value is of the expected type, otherwise returns
///     `None`.
/// - `IsVar`
///     * `fn is_<variant>(&self) -> bool`
///     Returns `true`, if the enum value is of the expected type, otherwise returns `false`.
///     * `fn is_<variant>_and(&self, f: impl FnOnce(<ref_inner>) -> bool) -> bool`
///     Returns `true`, if the enum value is of the expected type and the given closure
///     evalutates to `true`, otherwise returns `false`.
/// - `VarAsRef`
///     * `fn <variant>_as_ref(&self) -> Option<<ref_inner>>`
///     Returns a reference to the inner data, if the enum value is of the expected type,
///     otherwise returns `None`.
///     * `fn <variant>_as_mut(&mut self) -> Option<<mut_inner>>`
///     Returns a mutable reference to the inner data, if the enum value is of the expected type,
///     otherwise returns `None`.
/// - `UnwrapVar`
///     * `fn unwrap_<variant>(self) -> <inner>`
///     Returns the inner data, if the enum value is of the expected type, otherwise panics.
///     * `fn unwrap_<variant>_as_ref(self) -> <inner>`
///     Returns a reference to the inner data, if the enum value is of the expected type,
///     otherwise panics.
///     * `fn unwrap_<variant>_as_mut(self) -> <inner>`
///     Returns a mutable reference to the inner data, if the enum value is of the expected type,
///     otherwise panics.
///     * `fn unwrap_<variant>_or(self, default: <inner>) -> <inner>`
///     Returns the inner data, if the enum value is of the expected type, otherwise returns the
///     given default value.
///     * `fn unwrap_<variant>_or_else(self, f: impl FnOnce(Self) -> <inner>) -> <inner>`
///     Returns the inner data, if the enum value is of the expected type, otherwise returns the
///     value that the given closure evaluated to.
/// - `ExpectVar`
///     * `fn expect_<variant>(self, msg: &str) -> <inner>`
///     Returns the inner data, if the enum is of the expected type, otherwise panics with the
///     given error message.
///     * `fn expect_<variant>_as_ref(self, msg: &str) -> <inner>`
///     Returns a reference to the inner data, if the enum is of the expected type, otherwise
///     panics with the given error message.
///     * `fn expect_<variant>_as_mut(self, msg: &str) -> <inner>`
///     Returns a mutable reference to the inner data, if the enum is of the expected type,
///     otherwise panics with the given error message.
///
/// This example will generate methods of the category `Var` and `IsVar`.
/// ```rust
/// # use enpow::enpow;
/// 
/// #[enpow(Var, IsVar)]
/// enum Test {
///     A,
///     B(u32),
///     C(u32, i32),
///     D { a: u32, b: i32 },
/// }
/// ```
///
/// This example will generate all methods.
/// ```rust
/// # use enpow::enpow;
/// 
/// #[enpow]
/// enum Test {
///     A,
///     B(u32),
///     C(u32, i32),
///     D { a: u32, b: i32 },
/// }
/// ```
/// 
/// ## Auto Derives
///
/// Attaching the additional attribute `enpow_derive()` __below__ `enpow` adds the specified auto
/// trait derives to the automatically generated types. `Ref` structs always automatically derive
/// `Clone` and `Copy`, while `RefMut` structs are prohibited from deriving these traits. This
/// exclusion will be handled automatically by the macro.
///
/// ```rust
/// # use enpow::enpow;
/// 
/// #[enpow]
/// #[enpow_derive(Clone, Debug, PartialEq)]
/// enum Test {
///     C(u32, i32),
///     D { a: u32, b: i32 },
/// }
/// ```
///////////////////////////////////////////////////////////////////////////////////////////////////
#[proc_macro_attribute]
pub fn enpow(
    attribute: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    match enpow2(attribute.into(), item.into()) {
        Ok(stream) => stream,
        Err(error) => error.to_compile_error(),
    }
    .into()
}

fn enpow2(attribute: TokenStream, item: TokenStream) -> Result<TokenStream, Error> {
    let types: Vec<_> = MethodType::from_attribute(attribute)?.into_iter().collect();

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
            let info: VarDeriveAttributeInfo = syn::parse2(attr.tokens.clone())?;
            self_derives.extend(info.derives);

            // Remove this attribute from the output ast
            output.attrs.remove(i - attr_removed);
            attr_removed += 1;
        }
    }

    // Ref already has Clone and Copy, while mut is not allowed to get Clone and Copy.
    // Because of the dynamic import system, we cannot say for sure how these macros are
    // identified -> best efford
    let derive_filter: [String; 6] = [
        quote!(core::clone::Clone).to_string(),
        quote!(clone::Clone).to_string(),
        quote!(Clone).to_string(),
        quote!(core::marker::Copy).to_string(),
        quote!(marker::Copy).to_string(),
        quote!(Copy).to_string(),
    ];
    let ref_derives: Vec<_> = self_derives
        .iter()
        .map(|path| (path.to_token_stream().to_string(), path))
        .filter_map(|(pathstr, path)| {
            if derive_filter.contains(&pathstr) {
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
    let visibility = &parent.visibility;
    let (gen_full, gen_short, gen_where) = parent.generics.split_for_impl();

    Ok(quote! {
        #output

        #(#[allow(unused)] #self_derives #self_defs)*
        #(#[allow(unused)] #ref_derives #[derive(Clone, Copy)] #ref_defs)*
        #(#[allow(unused)] #ref_derives #mut_defs)*

        #[automatically_derived]
        #[allow(unused)]
        impl #gen_full #enum_ident #gen_short #gen_where {
            #(#visibility #methods)*
        }
    })
}

#[cfg(test)]
mod tests {
    use crate::helper::MethodType;
    use proc_macro2::TokenStream;
    use std::str::FromStr;

    #[test]
    fn enpow_wrong_target() {
        let source = "struct A;";
        let input = TokenStream::from_str(source).unwrap();
        let result = super::generate(input, &[]);
        assert!(result.is_err());
    }

    #[test]
    fn enpow() {
        let source = "#[enpow_derive(Debug, PartialEq)]
            #[derive(Clone, Debug, PartialEq)]
            pub enum Token<Span> {
                Plus(Span),
                Minus(Span),
                Number {
                    span: Span,
                    value: u64,
                }
            }";
        let input = TokenStream::from_str(source).unwrap();
        let result =
            super::generate(input, &[MethodType::Variant, MethodType::VariantAsRef]).unwrap();

        println!("{result}");
    }
}
