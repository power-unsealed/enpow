use outer::*;

mod outer {
    use enpow::extract;

    #[extract(All)]
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
    // Use automatically generated structs
    InnerA;
    InnerB(0);
    InnerC('a', 0);
    InnerD { a: 'a', b: 0 };

    // Use automatic From implementation
    assert_eq!(Inner::from(InnerA), Inner::A::<u32, char>(InnerA));
    assert_eq!(Inner::from(InnerB(0)), Inner::B::<u32, char>(InnerB(0)));
    assert_eq!(Inner::from(InnerC('c', 0)), Inner::C(InnerC('c', 0)));
    assert_eq!(Inner::from(InnerD { a: 0, b: 'd' }), Inner::D(InnerD { a: 0, b: 'd' }));
}
