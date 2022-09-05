use outer::*;

mod outer {
    use enpow::{enpow, extract};

    #[extract(Single, Unnamed)]
    #[enpow(IsVar)]
    #[inner(derive(Debug, PartialEq))]
    #[derive(Debug, PartialEq)]
    pub enum Inner<T, S: ToString> {
        /// Docs for `A`
        A,
        /// Docs for `B`
        B(
            /// Docs for `B::0`
            T,
        ),
        /// Docs for `C`
        C(
            /// Docs for `C::0`
            T,
            /// Docs for `C::1`
            S,
        ),
        /// Docs for `D`
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
    assert!(Inner::<i32, char>::from(InnerB(0)).is_b());
    assert!(Inner::from(InnerC(0, 'c')).is_c());

    // Check whether inner(derive()) is applied to both macros
    assert_eq!(Inner::from(InnerC(0, 'c')), Inner::C(InnerC(0, 'c')));
    assert_eq!(Inner::D { a: 0, b: 'd' }, Inner::D { a: 0, b: 'd' });
}
