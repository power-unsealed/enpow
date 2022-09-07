use enpow::enpow;

#[enpow(UnwrapVar, ExpectVar, VarAsRef)]
#[inner(type_names=Var, derive(Debug, PartialEq))]
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
        Some(DRef { string: &"Hello" })
    );
    
    assert_eq!(
        Test::D { string: "Hello" }.unwrap_d_as_ref(),
        DRef { string: &"Hello" }
    );
    
    assert_eq!(
        Test::D { string: "Hello" }.expect_d_as_ref("Expected Test::D"),
        DRef { string: &"Hello" }
    );
}
