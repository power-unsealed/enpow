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
///     Returns the inner data, if the enum value is of the expected type, otherwise returns
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
/// This example will generate all methods.
/// ```rust
/// # use enpow::enpow;
///
/// #[enpow(All)]
/// #[inner(derive(Debug, PartialEq))]
/// #[derive(Debug, PartialEq)]
/// enum IpAddress {
///     None,
///     V4(u8, u8, u8, u8),
///     V6(String),
///     Multi {
///         v4: (u8, u8, u8, u8),
///         v6: String,
///     },
/// }
///
/// // fn <variant>()
/// assert_eq!(IpAddress::V4(192, 168, 0, 1).v4(), Some((192, 168, 0, 1)));
/// assert_eq!(IpAddress::V6("::1".into()).v6(), Some("::1".into()));
/// assert_eq!(IpAddress::None.multi(), None);
///
/// // fn is_<variant>()
/// assert_eq!(IpAddress::None.is_none(), true);
/// assert_eq!(IpAddress::V6("::1".into()).is_v4(), false);
///
/// // fn is_<variant>_and()
/// assert_eq!(IpAddress::V4(192, 168, 0, 1).is_v4_and(|ip| *ip.0 == 192), true);
/// assert_eq!(IpAddress::V6("::1".into()).is_v6_and(|ip| *ip == "::"), false);
/// assert_eq!(IpAddress::None.is_v4_and(|_| true), false);
///
/// // fn <variant>_as_ref()
/// assert_eq!(IpAddress::V4(192, 168, 0, 1).v4_as_ref(), Some((&192, &168, &0, &1)));
/// assert_eq!(
///     IpAddress::Multi { v4: (0, 0, 0, 0), v6: "::".into() }.multi_as_ref(),
///     Some(IpAddressMultiRef { v4: &(0, 0, 0, 0), v6: &"::".into() })
/// );
/// assert_eq!(IpAddress::V6("::1".into()).none_as_ref(), None);
///
/// // fn <variant>_as_mut()
/// let mut ip = IpAddress::V4(192, 168, 0, 1);
/// if let Some(v4) = ip.v4_as_mut() {
///     *v4.3 = 2;
/// }
/// assert_eq!(ip, IpAddress::V4(192, 168, 0, 2));
///
/// // fn unwrap_<variant>()
/// assert_eq!(IpAddress::V6("::1".into()).unwrap_v6(), "::1".to_owned());
///
/// // fn unwrap_<variant>_as_ref()
/// assert_eq!(IpAddress::V4(192, 168, 0, 1).unwrap_v4_as_ref(), (&192, &168, &0, &1));
///
/// // fn unwrap_<variant>_as_mut()
/// let mut ip = IpAddress::V4(192, 168, 0, 1);
/// *ip.unwrap_v4_as_mut().3 = 2;
/// assert_eq!(ip, IpAddress::V4(192, 168, 0, 2));
///
/// // fn unwrap_<variant>_or()
/// assert_eq!(IpAddress::V6("::1".into()).unwrap_v6_or("::".into()), "::1".to_owned());
/// assert_eq!(IpAddress::V4(192, 168, 0, 2).unwrap_v6_or("::".into()), "::".to_owned());
///
/// // fn unwrap_<variant>_or_else()
/// assert_eq!(IpAddress::None.unwrap_v4_or_else(|_| (0, 0, 0, 0)), (0, 0, 0, 0));
/// assert_eq!(
///     IpAddress::V6("::1".into()).unwrap_v6_or_else(|_| unreachable!()),
///     "::1".to_owned()
/// );
///
/// // fn expect_<variant>()
/// assert_eq!(IpAddress::V4(192, 168, 0, 1).expect_v4("Expected V4"), (192, 168, 0, 1));
///
/// // fn unwrap_<variant>_as_ref()
/// assert_eq!(
///     IpAddress::V6("::1".into()).expect_v6_as_ref("Unexpected variant"),
///     &"::1".to_owned()
/// );
///
/// // fn unwrap_<variant>_as_mut()
/// let mut ip = IpAddress::V6("::".into());
/// ip.expect_v6_as_mut("Expected V6").push('1');
/// assert_eq!(ip, IpAddress::V6("::1".into()));
/// ```
///
/// This example will generate methods of the category `Var` and `IsVar`.
/// ```rust
/// # use enpow::enpow;
///
/// #[enpow(Var, IsVar)]
/// #[inner(derive(Debug, PartialEq))]
/// #[derive(Debug, PartialEq)]
/// enum IpAddress {
///     None,
///     V4(u8, u8, u8, u8),
///     V6(String),
///     Multi {
///         v4: (u8, u8, u8, u8),
///         v6: String,
///     },
/// }
///
/// // fn <variant>()
/// assert_eq!(IpAddress::V4(192, 168, 0, 1).v4(), Some((192, 168, 0, 1)));
/// assert_eq!(IpAddress::None.multi(), None);
///
/// // fn is_<variant>()
/// assert_eq!(IpAddress::None.is_none(), true);
/// assert_eq!(IpAddress::V6("::1".into()).is_v4(), false);
///
/// // fn is_<variant>_and()
/// assert_eq!(IpAddress::V4(192, 168, 0, 1).is_v4_and(|ip| *ip.0 == 192), true);
/// assert_eq!(IpAddress::V6("::1".into()).is_v6_and(|ip| *ip == "::"), false);
/// assert_eq!(IpAddress::None.is_v4_and(|_| true), false);
/// ```
///
/// ## Configuration with `inner`
///
/// Attaching the additional configuration attribute `inner()` with the argument `derive()`
/// __below__ `enpow` enables to add auto trait derives to the automatically generated types. `Ref`
/// structs always automatically derive `Clone` and `Copy`, while `Mut` structs are prohibited from
/// deriving these traits. This exclusion will be handled automatically by the macro. `inner()`
/// also allows for renaming of the generated types and methods on a per-variant base. The syntax
/// for this is `type_name="NewTypeName"` and `method_name="new_method_name"`.
///
/// ```rust
/// # use enpow::enpow;
///
/// #[enpow(All)]
/// #[inner(derive(Debug, PartialEq))]
/// enum IpAddress {
///     None,
///     V4(u8, u8, u8, u8),
///     V6(String),
///     #[inner(type_name="MultiAddress", method_name="mul", derive(Clone))]
///     Multi {
///         v4: (u8, u8, u8, u8),
///         v6: String,
///     },
/// }
///
/// // Using PartialEq, Debug, and Clone derive
/// assert_eq!(
///     IpAddress::Multi { v4: (0, 0, 0, 0), v6: "::".into() }.unwrap_mul(),
///     MultiAddress { v4: (0, 0, 0, 0), v6: "::".into() }.clone()
/// );
///
/// // Using automatic Copy derive on Ref struct
/// let ip = IpAddress::Multi { v4: (0, 0, 0, 0), v6: "::".into() };
/// let copy = ip.unwrap_multi_as_ref();
/// let another_copy = copy;
/// assert_eq!(copy, IpAddressMultiRef { v4: &(0, 0, 0, 0), v6: &"::".into() });
/// assert_eq!(another_copy, IpAddressMultiRef { v4: &(0, 0, 0, 0), v6: &"::".into() });
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
/// generated structs. The macro also automatically implements the `From` trait for every extracted
/// type to convert it into its corresponding enum variant.
///
/// In parethesis, the following arguments to `extract` can be used to specify which variant types
/// to extract. Without any arguments, all variants will be extracted.
///
/// - `Unit`: Extracts variants without data into unit structs.
/// - `Single`: Extracts variants with a single unnamed field into tuple structs.
/// - `Unnamed`: Extracts variants with multiple unnamed fields into tuple structs.
/// - `Named`: Extracts variants with named fields into structs.
/// - `All`: Extracts all variants into structs.
///
/// This example will extract all variants.
/// 
/// ```rust
/// # use enpow::extract;
///
/// #[extract(All)]
/// enum IpAddress {
///     None,
///     V4(u8, u8, u8, u8),
///     V6(String),
///     Multi {
///         v4: (u8, u8, u8, u8),
///         v6: String,
///     },
/// }
///
/// // Using the modified enum variants and its generated structs
/// IpAddress::None(IpAddressNone);
/// IpAddress::V4(IpAddressV4(192, 168, 0, 1));
/// IpAddress::V6(IpAddressV6("::1".into()));
/// IpAddress::Multi(IpAddressMulti { v4: (192, 168, 0, 1), v6: "::1".into() });
/// ```
///
/// This example will extract all variants with multiple unnamed fields or named fields into
/// separate structs.
///
/// ```rust
/// # use enpow::extract;
///
/// #[extract(Unnamed, Named)]
/// enum IpAddress {
///     None,
///     V4(u8, u8, u8, u8),
///     V6(String),
///     Multi {
///         v4: (u8, u8, u8, u8),
///         v6: String,
///     },
/// }
///
/// // Using the unmodified enum variants
/// IpAddress::None;
/// IpAddress::V6("::1".into());
///
/// // Using the modified enum variants and its generated structs
/// IpAddress::V4(IpAddressV4(192, 168, 0, 1));
/// IpAddress::Multi(IpAddressMulti { v4: (192, 168, 0, 1), v6: "::1".into() });
/// ```
///
/// An additional `derive` macro attached to the enum should come __after__ `extract`
/// to make sure the automatically derived implementations match the changed enum structure.
///
/// ## Configuration with `inner`
///
/// Attaching the additional configuration attribute `inner()` with the argument `derive()`
/// __below__ `extract` enables to add auto trait derives to the automatically generated types.
/// `inner()` also allows for renaming of the generated types on a per-variant base. The syntax
/// for this is `type_name="NewTypeName"`.
///
/// ```rust
/// # use enpow::extract;
///
/// #[extract]
/// #[inner(derive(Clone, Debug, PartialEq))]
/// enum IpAddress {
///     #[inner(type_name="NoIp"), derive(Copy)]
///     None,
///     #[inner(type_name="IpV4"), derive(Copy)]
///     V4(u8, u8, u8, u8),
///     #[inner(type_name="IpV6")]
///     V6(String),
///     #[inner(type_name="MultiIp")]
///     Multi {
///         v4: (u8, u8, u8, u8),
///         v6: String,
///     },
/// }
///
/// // Using PartialEq and Debug derive
/// assert_eq!(
///     IpAddressMulti { v4: (0, 0, 0, 0), v6: "::".into() },
///     IpAddressMulti { v4: (0, 0, 0, 0), v6: "::".into() }
/// );
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
