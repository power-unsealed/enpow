use outer::*;

mod outer {
    use enpow::{enpow, extract};

    #[extract(Single, Unnamed)]
    #[enpow(IsVar, UnwrapVar)]
    #[inner(derive(Debug, PartialEq))]
    #[derive(Debug, PartialEq)]
    pub enum Inner<T, S: ToString> {
        /// Docs for `A`
        A,
        /// Docs for `B`
        #[inner(method_name="bee")]
        B(
            /// Docs for `B::0`
            T,
        ),
        /// Docs for `C`
        #[inner(type_name="C", method_name="sea")]
        C(
            /// Docs for `C::0`
            T,
            /// Docs for `C::1`
            S,
        ),
        /// Docs for `D`
        #[inner(type_name="D")]
        D {
            /// Docs for `D::a`
            a: T,
            /// Docs for `D::b`
            b: S,
        },
    }
}

#[test]
fn test() {
    assert!(Inner::<i32, char>::A.is_a());
    assert!(Inner::<i32, char>::from(InnerB(0)).is_bee());
    assert!(Inner::from(C(0, 'c')).is_sea());

    // Check whether inner(derive()) is applied to both macros
    assert_eq!(Inner::from(C(0, 'c')), Inner::C(C(0, 'c')));
    assert_eq!(Inner::from(D { a: 0, b: 'd' }).unwrap_d(), D { a: 0, b: 'd' });
}
