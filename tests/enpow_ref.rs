use enpow::enpow;

#[enpow(VarAsRef)]
#[inner(derive(Debug, PartialEq))]
pub enum Test<'a> {
    A,
    B(&'a str),
    C(&'a str, usize),
    D { string: &'a str },
}

#[test]
fn test() {
    assert_eq!(
        Test::D { string: "Hello" }.d_as_ref(),
        Some(TestDRef { string: &"Hello" })
    );
}
