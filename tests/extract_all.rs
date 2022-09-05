use outer::*;

mod outer {
    use enpow::extract;

    #[extract(All)]
    #[inner(derive(Debug, PartialEq))]
    #[derive(Debug, PartialEq)]
    pub enum Inner<T, S: ToString> {
        /// Docs for `A`
        #[inner(type_name="A")]
        A,
        /// Docs for `B`
        #[inner(type_name="B")]
        B(
            /// Docs for `B::0`
            T,
        ),
        /// Docs for `C`
        #[inner(type_name="C")]
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
#[allow(path_statements)]
fn test() {
    // Use automatically generated structs
    A;
    B(0);
    C('a', 0);
    D { a: 'a', b: 0 };

    // Use automatic From implementation
    assert_eq!(Inner::from(A), Inner::A::<u32, char>(A));
    assert_eq!(Inner::from(B(0)), Inner::B::<u32, char>(B(0)));
    assert_eq!(Inner::from(C('c', 0)), Inner::C(C('c', 0)));
    assert_eq!(Inner::from(D { a: 0, b: 'd' }), Inner::D(D { a: 0, b: 'd' }));
}
