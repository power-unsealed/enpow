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
/// #[enpow_derive(Debug, PartialEq)]
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
/// <details>
/// <summary>ℹ️ Click to reveal generated code</summary>
///
/// ```rust
/// #[derive(Debug, PartialEq)]
/// enum IpAddress {
///     None,
///     V4(u8, u8, u8, u8),
///     V6(String),
///     Multi { v4: (u8, u8, u8, u8), v6: String },
/// }
/// 
/// #[allow(unused)]
/// #[derive(Debug, PartialEq)]
/// struct IpAddressMulti {
///     pub v4: (u8, u8, u8, u8),
///     pub v6: String,
/// }
/// 
/// #[allow(unused)]
/// #[derive(Debug, PartialEq, Clone, Copy)]
/// struct IpAddressMultiRef<'ip_address_multi> {
///     pub v4: &'ip_address_multi (u8, u8, u8, u8),
///     pub v6: &'ip_address_multi String,
/// }
/// 
/// #[allow(unused)]
/// #[derive(Debug, PartialEq)]
/// struct IpAddressMultiMut<'ip_address_multi> {
///     pub v4: &'ip_address_multi mut (u8, u8, u8, u8),
///     pub v6: &'ip_address_multi mut String,
/// }
/// 
/// #[automatically_derived]
/// #[allow(unused)]
/// impl IpAddress {
///     /// Returns the inner data, if the enum value is of the expected type, otherwise
///     /// returns `None`.
///     fn none(self) -> Option<()> {
///         match self {
///             IpAddress::None => Some(()),
///             _ => None,
///         }
///     }
///     
///     /// Returns a reference to the inner data, if the enum value is of the expected type,
///     /// otherwise returns `None`.
///     fn none_as_ref<'ip_address_none>(&'ip_address_none self) -> Option<()> {
///         match self {
///             IpAddress::None => Some(()),
///             _ => None,
///         }
///     }
///     
///     /// Returns a mutable reference to the inner data, if the enum value is of the expected
///     /// type, otherwise returns `None`.
///     fn none_as_mut<'ip_address_none>(&'ip_address_none mut self) -> Option<()> {
///         match self {
///             IpAddress::None => Some(()),
///             _ => None,
///         }
///     }
///     
///     /// Returns `true`, if the enum value is of the expected type, otherwise returns
///     /// `false`.
///     fn is_none(&self) -> bool {
///         match self {
///             IpAddress::None => true,
///             _ => false,
///         }
///     }
///     
///     /// Returns `true`, if the enum value is of the expected type and the given closure
///     /// evalutates to `true`, otherwise returns `false`.
///     fn is_none_and<'ip_address_none>(&'ip_address_none self, f: impl FnOnce(()) -> bool) -> bool {
///         match self {
///             IpAddress::None => f(()),
///             _ => false,
///         }
///     }
///     
///     /// Returns the inner data, if the enum value is of the expected type, otherwise
///     /// panics.
///     fn unwrap_none(self) -> () {
///         match self {
///             IpAddress::None => (),
///             _ => panic!("Failed unwrapping to IpAddress::None. Unexpected variant"),
///         }
///     }
///     
///     /// Returns a reference to the inner data, if the enum value is of the expected type,
///     /// otherwise panics.
///     fn unwrap_none_as_ref<'ip_address_none>(&'ip_address_none self) -> () {
///         match self {
///             IpAddress::None => (),
///             _ => panic!("Failed unwrapping to IpAddress::None. Unexpected variant"),
///         }
///     }
///     
///     /// Returns a mutable reference to the inner data, if the enum value is of the expected
///     /// type, otherwise panics.
///     fn unwrap_none_as_mut<'ip_address_none>(&'ip_address_none mut self) -> () {
///         match self {
///             IpAddress::None => (),
///             _ => panic!("Failed unwrapping to IpAddress::None. Unexpected variant"),
///         }
///     }
///     
///     /// Returns the inner data, if the enum value is of the expected type, otherwise
///     /// returns the given default value.
///     fn unwrap_none_or(self, default: ()) -> () {
///         match self {
///             IpAddress::None => (),
///             _ => default,
///         }
///     }
///     
///     /// Returns the inner data, if the enum value is of the expected type, otherwise
///     /// returns the value that the given closure evaluated to.
///     fn unwrap_none_or_else(self, f: impl FnOnce(Self) -> ()) -> () {
///         match self {
///             IpAddress::None => (),
///             some => f(some),
///         }
///     }
///     
///     /// Returns the inner data, if the enum is of the expected type, otherwise panics with
///     /// the given error message.
///     fn expect_none(self, msg: &str) -> () {
///         match self {
///             IpAddress::None => (),
///             _ => panic!("{}", msg),
///         }
///     }
///     
///     /// Returns a reference to the inner data, if the enum is of the expected type,
///     /// otherwise panics with the given error message.
///     fn expect_none_as_ref<'ip_address_none>(&'ip_address_none self, msg: &str) -> () {
///         match self {
///             IpAddress::None => (),
///             _ => panic!("{}", msg),
///         }
///     }
///     
///     /// Returns a mutable reference to the inner data, if the enum is of the expected type,
///     /// otherwise panics with the given error message.
///     fn expect_none_as_mut<'ip_address_none>(&'ip_address_none mut self, msg: &str) -> () {
///         match self {
///             IpAddress::None => (),
///             _ => panic!("{}", msg),
///         }
///     }
///     
///     /// Returns the inner data, if the enum value is of the expected type, otherwise
///     /// returns `None`.
///     fn v4(self) -> Option<(u8, u8, u8, u8)> {
///         match self {
///             IpAddress::V4(f0, f1, f2, f3) => Some((f0, f1, f2, f3)),
///             _ => None,
///         }
///     }
///     
///     /// Returns a reference to the inner data, if the enum value is of the expected type,
///     /// otherwise returns `None`.
///     fn v4_as_ref<'ip_address_v4>(
///         &'ip_address_v4 self,
///     ) -> Option<(
///         &'ip_address_v4 u8,
///         &'ip_address_v4 u8,
///         &'ip_address_v4 u8,
///         &'ip_address_v4 u8,
///     )> {
///         match self {
///             IpAddress::V4(f0, f1, f2, f3) => Some((f0, f1, f2, f3)),
///             _ => None,
///         }
///     }
///     
///     /// Returns a mutable reference to the inner data, if the enum value is of the expected
///     /// type, otherwise returns `None`.
///     fn v4_as_mut<'ip_address_v4>(
///         &'ip_address_v4 mut self,
///     ) -> Option<(
///         &'ip_address_v4 mut u8,
///         &'ip_address_v4 mut u8,
///         &'ip_address_v4 mut u8,
///         &'ip_address_v4 mut u8,
///     )> {
///         match self {
///             IpAddress::V4(f0, f1, f2, f3) => Some((f0, f1, f2, f3)),
///             _ => None,
///         }
///     }
///     
///     /// Returns `true`, if the enum value is of the expected type, otherwise returns
///     /// `false`.
///     fn is_v4(&self) -> bool {
///         match self {
///             IpAddress::V4(f0, f1, f2, f3) => true,
///             _ => false,
///         }
///     }
///     
///     /// Returns `true`, if the enum value is of the expected type and the given closure
///     /// evalutates to `true`, otherwise returns `false`.
///     fn is_v4_and<'ip_address_v4>(
///         &'ip_address_v4 self,
///         f: impl FnOnce(
///             (
///                 &'ip_address_v4 u8,
///                 &'ip_address_v4 u8,
///                 &'ip_address_v4 u8,
///                 &'ip_address_v4 u8,
///             ),
///         ) -> bool,
///     ) -> bool {
///         match self {
///             IpAddress::V4(f0, f1, f2, f3) => f((f0, f1, f2, f3)),
///             _ => false,
///         }
///     }
///     
///     /// Returns the inner data, if the enum value is of the expected type, otherwise
///     /// panics.
///     fn unwrap_v4(self) -> (u8, u8, u8, u8) {
///         match self {
///             IpAddress::V4(f0, f1, f2, f3) => (f0, f1, f2, f3),
///             _ => panic!("Failed unwrapping to IpAddress::V4. Unexpected variant"),
///         }
///     }
///     
///     /// Returns a reference to the inner data, if the enum value is of the expected type,
///     /// otherwise panics.
///     fn unwrap_v4_as_ref<'ip_address_v4>(
///         &'ip_address_v4 self,
///     ) -> (
///         &'ip_address_v4 u8,
///         &'ip_address_v4 u8,
///         &'ip_address_v4 u8,
///         &'ip_address_v4 u8,
///     ) {
///         match self {
///             IpAddress::V4(f0, f1, f2, f3) => (f0, f1, f2, f3),
///             _ => panic!("Failed unwrapping to IpAddress::V4. Unexpected variant"),
///         }
///     }
///     
///     /// Returns a mutable reference to the inner data, if the enum value is of the expected
///     /// type, otherwise panics.
///     fn unwrap_v4_as_mut<'ip_address_v4>(
///         &'ip_address_v4 mut self,
///     ) -> (
///         &'ip_address_v4 mut u8,
///         &'ip_address_v4 mut u8,
///         &'ip_address_v4 mut u8,
///         &'ip_address_v4 mut u8,
///     ) {
///         match self {
///             IpAddress::V4(f0, f1, f2, f3) => (f0, f1, f2, f3),
///             _ => panic!("Failed unwrapping to IpAddress::V4. Unexpected variant"),
///         }
///     }
///     
///     /// Returns the inner data, if the enum value is of the expected type, otherwise
///     /// returns the given default value.
///     fn unwrap_v4_or(self, default: (u8, u8, u8, u8)) -> (u8, u8, u8, u8) {
///         match self {
///             IpAddress::V4(f0, f1, f2, f3) => (f0, f1, f2, f3),
///             _ => default,
///         }
///     }
///     
///     /// Returns the inner data, if the enum value is of the expected type, otherwise
///     /// returns the value that the given closure evaluated to.
///     fn unwrap_v4_or_else(self, f: impl FnOnce(Self) -> (u8, u8, u8, u8)) -> (u8, u8, u8, u8) {
///         match self {
///             IpAddress::V4(f0, f1, f2, f3) => (f0, f1, f2, f3),
///             some => f(some),
///         }
///     }
///     
///     /// Returns the inner data, if the enum is of the expected type, otherwise panics with
///     /// the given error message.
///     fn expect_v4(self, msg: &str) -> (u8, u8, u8, u8) {
///         match self {
///             IpAddress::V4(f0, f1, f2, f3) => (f0, f1, f2, f3),
///             _ => panic!("{}", msg),
///         }
///     }
///     
///     /// Returns a reference to the inner data, if the enum is of the expected type,
///     /// otherwise panics with the given error message.
///     fn expect_v4_as_ref<'ip_address_v4>(
///         &'ip_address_v4 self,
///         msg: &str,
///     ) -> (
///         &'ip_address_v4 u8,
///         &'ip_address_v4 u8,
///         &'ip_address_v4 u8,
///         &'ip_address_v4 u8,
///     ) {
///         match self {
///             IpAddress::V4(f0, f1, f2, f3) => (f0, f1, f2, f3),
///             _ => panic!("{}", msg),
///         }
///     }
///     
///     /// Returns a mutable reference to the inner data, if the enum is of the expected type,
///     /// otherwise panics with the given error message.
///     fn expect_v4_as_mut<'ip_address_v4>(
///         &'ip_address_v4 mut self,
///         msg: &str,
///     ) -> (
///         &'ip_address_v4 mut u8,
///         &'ip_address_v4 mut u8,
///         &'ip_address_v4 mut u8,
///         &'ip_address_v4 mut u8,
///     ) {
///         match self {
///             IpAddress::V4(f0, f1, f2, f3) => (f0, f1, f2, f3),
///             _ => panic!("{}", msg),
///         }
///     }
///     
///     /// Returns the inner data, if the enum value is of the expected type, otherwise
///     /// returns `None`.
///     fn v6(self) -> Option<String> {
///         match self {
///             IpAddress::V6(f0) => Some(f0),
///             _ => None,
///         }
///     }
///     
///     /// Returns a reference to the inner data, if the enum value is of the expected type,
///     /// otherwise returns `None`.
///     fn v6_as_ref<'ip_address_v6>(&'ip_address_v6 self) -> Option<&'ip_address_v6 String> {
///         match self {
///             IpAddress::V6(f0) => Some(f0),
///             _ => None,
///         }
///     }
///     
///     /// Returns a mutable reference to the inner data, if the enum value is of the expected
///     /// type, otherwise returns `None`.
///     fn v6_as_mut<'ip_address_v6>(&'ip_address_v6 mut self) -> Option<&'ip_address_v6 mut String> {
///         match self {
///             IpAddress::V6(f0) => Some(f0),
///             _ => None,
///         }
///     }
///     
///     /// Returns `true`, if the enum value is of the expected type, otherwise returns
///     /// `false`.
///     fn is_v6(&self) -> bool {
///         match self {
///             IpAddress::V6(f0) => true,
///             _ => false,
///         }
///     }
///     
///     /// Returns `true`, if the enum value is of the expected type and the given closure
///     /// evalutates to `true`, otherwise returns `false`.
///     fn is_v6_and<'ip_address_v6>(
///         &'ip_address_v6 self,
///         f: impl FnOnce(&'ip_address_v6 String) -> bool,
///     ) -> bool {
///         match self {
///             IpAddress::V6(f0) => f(f0),
///             _ => false,
///         }
///     }
///     
///     /// Returns the inner data, if the enum value is of the expected type, otherwise
///     /// panics.
///     fn unwrap_v6(self) -> String {
///         match self {
///             IpAddress::V6(f0) => f0,
///             _ => panic!("Failed unwrapping to IpAddress::V6. Unexpected variant"),
///         }
///     }
///     
///     /// Returns a reference to the inner data, if the enum value is of the expected type,
///     /// otherwise panics.
///     fn unwrap_v6_as_ref<'ip_address_v6>(&'ip_address_v6 self) -> &'ip_address_v6 String {
///         match self {
///             IpAddress::V6(f0) => f0,
///             _ => panic!("Failed unwrapping to IpAddress::V6. Unexpected variant"),
///         }
///     }
///     
///     /// Returns a mutable reference to the inner data, if the enum value is of the expected
///     /// type, otherwise panics.
///     fn unwrap_v6_as_mut<'ip_address_v6>(&'ip_address_v6 mut self) -> &'ip_address_v6 mut String {
///         match self {
///             IpAddress::V6(f0) => f0,
///             _ => panic!("Failed unwrapping to IpAddress::V6. Unexpected variant"),
///         }
///     }
///     
///     /// Returns the inner data, if the enum value is of the expected type, otherwise
///     /// returns the given default value.
///     fn unwrap_v6_or(self, default: String) -> String {
///         match self {
///             IpAddress::V6(f0) => f0,
///             _ => default,
///         }
///     }
///     
///     /// Returns the inner data, if the enum value is of the expected type, otherwise
///     /// returns the value that the given closure evaluated to.
///     fn unwrap_v6_or_else(self, f: impl FnOnce(Self) -> String) -> String {
///         match self {
///             IpAddress::V6(f0) => f0,
///             some => f(some),
///         }
///     }
///     
///     /// Returns the inner data, if the enum is of the expected type, otherwise panics with
///     /// the given error message.
///     fn expect_v6(self, msg: &str) -> String {
///         match self {
///             IpAddress::V6(f0) => f0,
///             _ => panic!("{}", msg),
///         }
///     }
///     
///     /// Returns a reference to the inner data, if the enum is of the expected type,
///     /// otherwise panics with the given error message.
///     fn expect_v6_as_ref<'ip_address_v6>(&'ip_address_v6 self, msg: &str) -> &'ip_address_v6 String {
///         match self {
///             IpAddress::V6(f0) => f0,
///             _ => panic!("{}", msg),
///         }
///     }
///     
///     /// Returns a mutable reference to the inner data, if the enum is of the expected type,
///     /// otherwise panics with the given error message.
///     fn expect_v6_as_mut<'ip_address_v6>(
///         &'ip_address_v6 mut self,
///         msg: &str,
///     ) -> &'ip_address_v6 mut String {
///         match self {
///             IpAddress::V6(f0) => f0,
///             _ => panic!("{}", msg),
///         }
///     }
///     
///     /// Returns the inner data, if the enum value is of the expected type, otherwise
///     /// returns `None`.
///     fn multi(self) -> Option<IpAddressMulti> {
///         match self {
///             IpAddress::Multi { v4, v6 } => Some(IpAddressMulti { v4, v6 }),
///             _ => None,
///         }
///     }
///     
///     /// Returns a reference to the inner data, if the enum value is of the expected type,
///     /// otherwise returns `None`.
///     fn multi_as_ref<'ip_address_multi>(
///         &'ip_address_multi self,
///     ) -> Option<IpAddressMultiRef<'ip_address_multi>> {
///         match self {
///             IpAddress::Multi { v4, v6 } => Some(IpAddressMultiRef { v4, v6 }),
///             _ => None,
///         }
///     }
///     
///     /// Returns a mutable reference to the inner data, if the enum value is of the expected
///     /// type, otherwise returns `None`.
///     fn multi_as_mut<'ip_address_multi>(
///         &'ip_address_multi mut self,
///     ) -> Option<IpAddressMultiMut<'ip_address_multi>> {
///         match self {
///             IpAddress::Multi { v4, v6 } => Some(IpAddressMultiMut { v4, v6 }),
///             _ => None,
///         }
///     }
///     
///     /// Returns `true`, if the enum value is of the expected type, otherwise returns
///     /// `false`.
///     fn is_multi(&self) -> bool {
///         match self {
///             IpAddress::Multi { v4, v6 } => true,
///             _ => false,
///         }
///     }
///     
///     /// Returns `true`, if the enum value is of the expected type and the given closure
///     /// evalutates to `true`, otherwise returns `false`.
///     fn is_multi_and<'ip_address_multi>(
///         &'ip_address_multi self,
///         f: impl FnOnce(IpAddressMultiRef<'ip_address_multi>) -> bool,
///     ) -> bool {
///         match self {
///             IpAddress::Multi { v4, v6 } => f(IpAddressMultiRef { v4, v6 }),
///             _ => false,
///         }
///     }
///     
///     /// Returns the inner data, if the enum value is of the expected type, otherwise
///     /// panics.
///     fn unwrap_multi(self) -> IpAddressMulti {
///         match self {
///             IpAddress::Multi { v4, v6 } => IpAddressMulti { v4, v6 },
///             _ => panic!("Failed unwrapping to IpAddress::Multi. Unexpected variant"),
///         }
///     }
///     
///     /// Returns a reference to the inner data, if the enum value is of the expected type,
///     /// otherwise panics.
///     fn unwrap_multi_as_ref<'ip_address_multi>(
///         &'ip_address_multi self,
///     ) -> IpAddressMultiRef<'ip_address_multi> {
///         match self {
///             IpAddress::Multi { v4, v6 } => IpAddressMultiRef { v4, v6 },
///             _ => panic!("Failed unwrapping to IpAddress::Multi. Unexpected variant"),
///         }
///     }
///     
///     /// Returns a mutable reference to the inner data, if the enum value is of the expected
///     /// type, otherwise panics.
///     fn unwrap_multi_as_mut<'ip_address_multi>(
///         &'ip_address_multi mut self,
///     ) -> IpAddressMultiMut<'ip_address_multi> {
///         match self {
///             IpAddress::Multi { v4, v6 } => IpAddressMultiMut { v4, v6 },
///             _ => panic!("Failed unwrapping to IpAddress::Multi. Unexpected variant"),
///         }
///     }
///     
///     /// Returns the inner data, if the enum value is of the expected type, otherwise
///     /// returns the given default value.
///     fn unwrap_multi_or(self, default: IpAddressMulti) -> IpAddressMulti {
///         match self {
///             IpAddress::Multi { v4, v6 } => IpAddressMulti { v4, v6 },
///             _ => default,
///         }
///     }
///     
///     /// Returns the inner data, if the enum value is of the expected type, otherwise
///     /// returns the value that the given closure evaluated to.
///     fn unwrap_multi_or_else(self, f: impl FnOnce(Self) -> IpAddressMulti) -> IpAddressMulti {
///         match self {
///             IpAddress::Multi { v4, v6 } => IpAddressMulti { v4, v6 },
///             some => f(some),
///         }
///     }
///     
///     /// Returns the inner data, if the enum is of the expected type, otherwise panics with
///     /// the given error message.
///     fn expect_multi(self, msg: &str) -> IpAddressMulti {
///         match self {
///             IpAddress::Multi { v4, v6 } => IpAddressMulti { v4, v6 },
///             _ => panic!("{}", msg),
///         }
///     }
///     
///     /// Returns a reference to the inner data, if the enum is of the expected type,
///     /// otherwise panics with the given error message.
///     fn expect_multi_as_ref<'ip_address_multi>(
///         &'ip_address_multi self,
///         msg: &str,
///     ) -> IpAddressMultiRef<'ip_address_multi> {
///         match self {
///             IpAddress::Multi { v4, v6 } => IpAddressMultiRef { v4, v6 },
///             _ => panic!("{}", msg),
///         }
///     }
///     
///     /// Returns a mutable reference to the inner data, if the enum is of the expected type,
///     /// otherwise panics with the given error message.
///     fn expect_multi_as_mut<'ip_address_multi>(
///         &'ip_address_multi mut self,
///         msg: &str,
///     ) -> IpAddressMultiMut<'ip_address_multi> {
///         match self {
///             IpAddress::Multi { v4, v6 } => IpAddressMultiMut { v4, v6 },
///             _ => panic!("{}", msg),
///         }
///     }
/// }
/// ```
/// </details>
///
/// This example will generate methods of the category `Var` and `IsVar`.
/// ```rust
/// # use enpow::enpow;
///
/// #[enpow(Var, IsVar)]
/// #[enpow_derive(Debug, PartialEq)]
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
/// #[enpow(All)]
/// #[enpow_derive(Debug, PartialEq)]
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
/// // Using PartialEq and Debug derive
/// assert_eq!(
///     IpAddress::Multi { v4: (0, 0, 0, 0), v6: "::".into() }.unwrap_multi(),
///     IpAddressMulti { v4: (0, 0, 0, 0), v6: "::".into() }
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
/// generated structs.
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
/// <details>
/// <summary>ℹ️ Click to reveal generated code</summary>
///
/// ```rust
/// enum IpAddress {
///     None(IpAddressNone),
///     V4(IpAddressV4),
///     V6(IpAddressV6),
///     Multi(IpAddressMulti),
/// }
/// 
/// #[derive()]
/// struct IpAddressNone;
/// 
/// #[derive()]
/// struct IpAddressV4(pub u8, pub u8, pub u8, pub u8);
/// 
/// #[derive()]
/// struct IpAddressV6(pub String);
/// 
/// #[derive()]
/// struct IpAddressMulti {
///     pub v4: (u8, u8, u8, u8),
///     pub v6: String,
/// }
/// ```
/// </details>
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
/// <details>
/// <summary>ℹ️ Click to reveal generated code</summary>
///
/// ```rust
/// enum IpAddress {
///     None,
///     V4(IpAddressV4),
///     V6(String),
///     Multi(IpAddressMulti),
/// }
/// 
/// #[derive()]
/// struct IpAddressV4(pub u8, pub u8, pub u8, pub u8);
/// 
/// #[derive()]
/// struct IpAddressMulti {
///     pub v4: (u8, u8, u8, u8),
///     pub v6: String,
/// }
/// ```
/// </details>
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
/// // Using PartialEq and Debug derive
/// assert_eq!(
///     IpAddressMulti { v4: (0, 0, 0, 0), v6: "::".into() },
///     IpAddressMulti { v4: (0, 0, 0, 0), v6: "::".into() }
/// );
/// ```
///
/// <details>
/// <summary>ℹ️ Click to reveal generated code</summary>
///
/// ```rust
/// enum IpAddress {
///     None(IpAddressNone),
///     V4(IpAddressV4),
///     V6(IpAddressV6),
///     Multi(IpAddressMulti),
/// }
/// 
/// #[derive(Clone, Debug, PartialEq)]
/// struct IpAddressNone;
/// 
/// #[derive(Clone, Debug, PartialEq)]
/// struct IpAddressV4(pub u8, pub u8, pub u8, pub u8);
/// 
/// #[derive(Clone, Debug, PartialEq)]
/// struct IpAddressV6(pub String);
/// 
/// #[derive(Clone, Debug, PartialEq)]
/// struct IpAddressMulti {
///     pub v4: (u8, u8, u8, u8),
///     pub v6: String,
/// }
/// ```
/// </details>
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
