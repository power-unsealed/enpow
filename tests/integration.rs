mod outer {
    use enpow::enpow;

    #[enpow(All)]
    #[var_derive(Debug, PartialEq)]
    pub enum Inner<T, S: ToString> {
        A,
        B(T),
        C(T, S),
        D { a: T, b: S },
    }
}

use outer::*;

#[test]
fn enpow() {
    assert_eq!(Inner::<i32, char>::A.a(), Some(()));
    assert_eq!(Inner::<i32, char>::B(0).b(), Some(0));
    assert_eq!(Inner::<i32, char>::C(0, 'c').c(), Some((0, 'c')));
    assert_eq!(Inner::D { a: 0, b: 'd' }.d(), Some(InnerD { a: 0, b: 'd' }));
}
