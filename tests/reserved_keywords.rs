use enpow::enpow;

#[enpow(All)]
#[inner(type_names=Var)]
enum Token {
    Enum,
    Fn,
    Struct,
    Mod,
}

#[test]
fn dummy() {
    Token::Enum.r#enum();
    Token::Fn.r#fn();
    Token::Struct.r#struct();
    Token::Mod.r#mod();
}
