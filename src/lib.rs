#![doc = include_str!("../README.md")]

mod enpow;
mod extract;
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
/// - `All`
///     * Generates all methods mentioned.
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
/// 
/// // Use the auto implementation
/// assert!(Test::A.is_a());
/// assert_eq!(Test::B(7).b(), Some(7));
/// ```
///
/// This example will generate all methods.
/// ```rust
/// # use enpow::enpow;
///
/// #[enpow]
/// #[derive(Debug, PartialEq)]
/// enum Test {
///     A,
///     B(u32),
///     C(u32, i32),
///     D { a: u32, b: i32 },
/// }
/// 
/// // Use the auto implementation
/// assert!(Test::A.is_a());
/// assert_eq!(Test::B(7).b(), Some(7));
/// Test::C(3, -3).expect_c("Expected Test::C");
/// 
/// let mut test = Test::D { a: 7, b: -7 };
/// if let Some(dmut) = test.d_as_mut() {
///     *dmut.b = 42;
/// }
/// assert_eq!(test, Test::D { a: 7, b: 42 });
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

///////////////////////////////////////////////////////////////////////////////////////////////////
/// The `extract` attribute attached to an enum turns each variant into a separate struct that then
/// becomes the only field of the variant. It supports generics and variants of every type, with
/// named or unnamed fields or no fields attached. Variants without data are turned into unit
/// structs, variants with unnamed fields get turned into tuple structs, and variants with named
/// fields are transformed into structs, each named after the enum and variant, i.e.
/// `EnumVariant`. The structs generated inherit the visibility modifier of the target enum.
/// Additionally, doc comments attached to the variants and variant fields are inherited by the
/// generated structs.
///
/// In parethesis, the following arguments to `extract` can be used to specify which variant types
/// to extract. Without any arguments, all variants will be extracted.
///
/// - `Unit`: Extracts variants without data into unit structs.
/// - `Single`: Extracts variants with a single unnamed field into tuple structs.
/// - `Unnamed`: Extracts variants with multiple unnamed fields into tuple structs.
/// - `Named`: Extracts variants with named fields into structs.
///
/// This example will extract all variants with multiple unnamed fields or named fields into
/// separate structs.
/// ```rust
/// # use enpow::extract;
///
/// #[extract(Unnamed, Named)]
/// enum Test {
///     A,
///     B(u32),
///     C(u32, i32),
///     D { a: u32, b: i32 },
/// }
///
/// Test::A;
/// Test::B(7);
/// Test::C(TestC(7, -7));
/// Test::D(TestD { a: 7, b: -7 });
/// ```
///
/// This example will extract all variants.
/// ```rust
/// # use enpow::extract;
///
/// #[extract]
/// enum Test {
///     A,
///     B(u32),
///     C(u32, i32),
///     D { a: u32, b: i32 },
/// }
///
/// Test::A(TestA);
/// Test::B(TestB(7));
/// Test::C(TestC(7, -7));
/// Test::D(TestD { a: 7, b: -7 });
/// ```
///
/// An additional `derive` macro attached to the enum should come __after__ `extract`
/// to make sure the automatically derived implementations match the changed enum structure.
///
/// ## Auto Derives
///
/// Attaching the additional attribute `extract_derive()` __below__ `extract` adds the specified
/// auto trait derives to the automatically generated types.
///
/// ```rust
/// # use enpow::extract;
///
/// #[extract]
/// #[extract_derive(Clone, Debug, PartialEq)]
/// enum Test {
///     C(u32, i32),
///     D { a: u32, b: i32 },
/// }
///
/// // Using PartialEq and Debug
/// assert_eq!(TestD { a: 7, b: -7 }, TestD { a: 7, b: -7 });
/// ```
///////////////////////////////////////////////////////////////////////////////////////////////////
#[proc_macro_attribute]
pub fn extract(
    attribute: proc_macro::TokenStream,
    item: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    match extract::entry(attribute.into(), item.into()) {
        Ok(stream) => stream,
        Err(error) => error.to_compile_error(),
    }
    .into()
}
