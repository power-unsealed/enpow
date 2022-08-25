//! EnPow is a procedural macro crate used to en**Pow**er user defined **En**ums with many methods
//! usually known from the standard library's `Result<T, E>` and `Option<T>`. It can generate
//! methods like `fn is_<variant>(&self) -> bool` or `fn unwrap_<variant>(self) -> <inner>`,
//! supporting variants with named or unnamed fields (or none), as well as generics. See the
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

mod enpow;
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
/// `Clone` and `Copy`, while `Mut` structs are prohibited from deriving these traits. This
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
/// 
/// // Using PartialEq and Debug
/// assert_eq!(Test::D { a: 7, b: -7 }.unwrap_d(), TestD { a: 7, b: -7 });
/// ```
///////////////////////////////////////////////////////////////////////////////////////////////////
#[proc_macro_attribute]
pub fn enpow(
    attribute: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    match enpow::entry(attribute.into(), item.into()) {
        Ok(stream) => stream,
        Err(error) => error.to_compile_error(),
    }
    .into()
}
