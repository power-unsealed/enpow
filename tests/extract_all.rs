mod outer {
    use enpow::extract;

    #[extract]
    #[extract_derive(Debug, PartialEq)]
    #[derive(Debug, PartialEq)]
    pub enum Inner<T, S: ToString> {
        A,
        B(T),
        C(T, S),
        D { a: T, b: S },
    }
}

#[test]
fn test() {
    outer::InnerA;
    outer::InnerB(0);
    outer::InnerC('a', 0);
    outer::InnerD { a: 'a', b: 0 };
}