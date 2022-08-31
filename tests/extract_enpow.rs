mod outer {
    use enpow::{enpow, extract};

    #[extract(Single, Unnamed, Named)]
    #[extract_derive(Debug, PartialEq)]
    #[enpow(IsVar)]
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
    assert!(outer::Inner::<i32, char>::A.is_a());
    assert!(outer::Inner::<i32, char>::B(outer::InnerB(0)).is_b());
    assert!(outer::Inner::<i32, char>::C(outer::InnerC(0, 'c')).is_c());
    assert!(outer::Inner::<i32, char>::D(outer::InnerD { a: 0, b: 'd' }).is_d());
}