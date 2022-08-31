mod outer {
    use enpow::{enpow, extract};

    #[extract]
    #[extract_derive(Debug, PartialEq)]
    #[enpow(All)]
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
    outer::Inner::<i32, char>::A(outer::InnerA);
    outer::Inner::<i32, char>::B(outer::InnerB(0));
    outer::Inner::<i32, char>::C(outer::InnerC(0, 'c'));
    outer::Inner::<i32, char>::D(outer::InnerD { a: 0, b: 'd' });
}