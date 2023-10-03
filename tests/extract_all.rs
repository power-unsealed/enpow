use outer::*;

mod outer {
    use enpow::extract;

    #[extract(All)]
    #[inner(type_names = "{enum}Var{var}", derive(Debug, PartialEq))]
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
        #[inner(type_name = "{var}Sea")]
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
    InnerVarA;
    InnerVarB(0);
    CSea('a', 0);
    InnerVarD { a: 'a', b: 0 };

    // Use automatic From implementation
    assert_eq!(Inner::from(InnerVarA), Inner::A::<u32, char>(InnerVarA));
    assert_eq!(
        Inner::from(InnerVarB(0)),
        Inner::B::<u32, char>(InnerVarB(0))
    );
    assert_eq!(Inner::from(CSea('c', 0)), Inner::C(CSea('c', 0)));
    assert_eq!(
        Inner::from(InnerVarD { a: 0, b: 'd' }),
        Inner::D(InnerVarD { a: 0, b: 'd' })
    );
}
