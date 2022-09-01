use enpow::enpow;

#[enpow(VarAsRef)]
pub enum Test<'a> {
    A {
        string: &'a str,
    }
}