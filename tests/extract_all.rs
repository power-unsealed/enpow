mod outer {
    use enpow::extract;

    #[extract]
    #[extract_derive(Debug, PartialEq)]
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
#[allow(path_statements)]
fn test() {
    outer::InnerA;
    outer::InnerB(0);
    outer::InnerC('a', 0);
    outer::InnerD { a: 'a', b: 0 };
}
