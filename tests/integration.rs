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
fn variant() {
    assert_eq!(Inner::<i32, char>::A.a(), Some(()));
    assert_eq!(Inner::<i32, char>::A.b(), None);
    assert_eq!(Inner::<i32, char>::A.c(), None);
    assert_eq!(Inner::<i32, char>::A.d(), None);
    assert_eq!(Inner::<i32, char>::B(0).b(), Some(0));
    assert_eq!(Inner::<i32, char>::B(0).c(), None);
    assert_eq!(Inner::<i32, char>::B(0).d(), None);
    assert_eq!(Inner::<i32, char>::B(0).a(), None);
    assert_eq!(Inner::<i32, char>::C(0, 'c').c(), Some((0, 'c')));
    assert_eq!(Inner::<i32, char>::C(0, 'c').d(), None);
    assert_eq!(Inner::<i32, char>::C(0, 'c').a(), None);
    assert_eq!(Inner::<i32, char>::C(0, 'c').b(), None);
    assert_eq!(Inner::D { a: 0, b: 'd' }.d(), Some(InnerD { a: 0, b: 'd' }));
    assert_eq!(Inner::D { a: 0, b: 'd' }.a(), None);
    assert_eq!(Inner::D { a: 0, b: 'd' }.b(), None);
    assert_eq!(Inner::D { a: 0, b: 'd' }.c(), None);
}

#[test]
fn is_variant() {
    assert_eq!(Inner::<i32, char>::A.is_a(), true);
    assert_eq!(Inner::<i32, char>::A.is_b(), false);
    assert_eq!(Inner::<i32, char>::A.is_c(), false);
    assert_eq!(Inner::<i32, char>::A.is_d(), false);
    assert_eq!(Inner::<i32, char>::B(0).is_b(), true);
    assert_eq!(Inner::<i32, char>::B(0).is_c(), false);
    assert_eq!(Inner::<i32, char>::B(0).is_d(), false);
    assert_eq!(Inner::<i32, char>::B(0).is_a(), false);
    assert_eq!(Inner::<i32, char>::C(0, 'c').is_c(), true);
    assert_eq!(Inner::<i32, char>::C(0, 'c').is_d(), false);
    assert_eq!(Inner::<i32, char>::C(0, 'c').is_a(), false);
    assert_eq!(Inner::<i32, char>::C(0, 'c').is_b(), false);
    assert_eq!(Inner::D { a: 0, b: 'd' }.is_d(), true);
    assert_eq!(Inner::D { a: 0, b: 'd' }.is_a(), false);
    assert_eq!(Inner::D { a: 0, b: 'd' }.is_b(), false);
    assert_eq!(Inner::D { a: 0, b: 'd' }.is_c(), false);
    
    assert_eq!(Inner::<i32, char>::A.is_a_and(|_| true), true);
    assert_eq!(Inner::<i32, char>::A.is_a_and(|_| false), false);
    assert_eq!(Inner::<i32, char>::A.is_b_and(|_| true), false);
    assert_eq!(Inner::<i32, char>::A.is_c_and(|_| true), false);
    assert_eq!(Inner::<i32, char>::A.is_d_and(|_| true), false);
    assert_eq!(Inner::<i32, char>::B(0).is_b_and(|b| *b == 0), true);
    assert_eq!(Inner::<i32, char>::B(0).is_b_and(|b| *b == 1), false);
    assert_eq!(Inner::<i32, char>::B(0).is_c_and(|_| true), false);
    assert_eq!(Inner::<i32, char>::B(0).is_d_and(|_| true), false);
    assert_eq!(Inner::<i32, char>::B(0).is_a_and(|_| true), false);
    assert_eq!(Inner::<i32, char>::C(0, 'c').is_c_and(|(x, y)| *x == 0 && *y == 'c'), true);
    assert_eq!(Inner::<i32, char>::C(0, 'c').is_c_and(|(x, y)| *x == 1 && *y == 'c'), false);
    assert_eq!(Inner::<i32, char>::C(0, 'c').is_d_and(|_| true), false);
    assert_eq!(Inner::<i32, char>::C(0, 'c').is_a_and(|_| true), false);
    assert_eq!(Inner::<i32, char>::C(0, 'c').is_b_and(|_| true), false);
    assert_eq!(Inner::D { a: 0, b: 'd' }.is_d_and(|d| *d.a == 0 && *d.b == 'd'), true);
    assert_eq!(Inner::D { a: 0, b: 'd' }.is_d_and(|d| *d.a == 0 && *d.b == 'a'), false);
    assert_eq!(Inner::D { a: 0, b: 'd' }.is_a_and(|_| true), false);
    assert_eq!(Inner::D { a: 0, b: 'd' }.is_b_and(|_| true), false);
    assert_eq!(Inner::D { a: 0, b: 'd' }.is_c_and(|_| true), false);
}

#[test]
fn variant_as_ref() {
    assert_eq!(Inner::<i32, char>::A.a_as_ref(), Some(()));
    assert_eq!(Inner::<i32, char>::A.b_as_ref(), None);
    assert_eq!(Inner::<i32, char>::A.c_as_ref(), None);
    assert_eq!(Inner::<i32, char>::A.d_as_ref(), None);
    assert_eq!(Inner::<i32, char>::B(0).b_as_ref(), Some(&0));
    assert_eq!(Inner::<i32, char>::B(0).c_as_ref(), None);
    assert_eq!(Inner::<i32, char>::B(0).d_as_ref(), None);
    assert_eq!(Inner::<i32, char>::B(0).a_as_ref(), None);
    assert_eq!(Inner::<i32, char>::C(0, 'c').c_as_ref(), Some((&0, &'c')));
    assert_eq!(Inner::<i32, char>::C(0, 'c').d_as_ref(), None);
    assert_eq!(Inner::<i32, char>::C(0, 'c').a_as_ref(), None);
    assert_eq!(Inner::<i32, char>::C(0, 'c').b_as_ref(), None);
    assert_eq!(Inner::D { a: 0, b: 'd' }.d_as_ref(), Some(InnerDRef { a: &0, b: &'d' }));
    assert_eq!(Inner::D { a: 0, b: 'd' }.a_as_ref(), None);
    assert_eq!(Inner::D { a: 0, b: 'd' }.b_as_ref(), None);
    assert_eq!(Inner::D { a: 0, b: 'd' }.c_as_ref(), None);
    
    assert_eq!(Inner::<i32, char>::A.a_as_mut(), Some(()));
    assert_eq!(Inner::<i32, char>::A.b_as_mut(), None);
    assert_eq!(Inner::<i32, char>::A.c_as_mut(), None);
    assert_eq!(Inner::<i32, char>::A.d_as_mut(), None);
    assert_eq!(Inner::<i32, char>::B(0).b_as_mut(), Some(&mut 0));
    assert_eq!(Inner::<i32, char>::B(0).c_as_mut(), None);
    assert_eq!(Inner::<i32, char>::B(0).d_as_mut(), None);
    assert_eq!(Inner::<i32, char>::B(0).a_as_mut(), None);
    assert_eq!(Inner::<i32, char>::C(0, 'c').c_as_mut(), Some((&mut 0, &mut 'c')));
    assert_eq!(Inner::<i32, char>::C(0, 'c').d_as_mut(), None);
    assert_eq!(Inner::<i32, char>::C(0, 'c').a_as_mut(), None);
    assert_eq!(Inner::<i32, char>::C(0, 'c').b_as_mut(), None);
    assert_eq!(Inner::D { a: 0, b: 'd' }.d_as_mut(), Some(InnerDRef { a: &mut 0, b: &mut 'd' }));
    assert_eq!(Inner::D { a: 0, b: 'd' }.a_as_mut(), None);
    assert_eq!(Inner::D { a: 0, b: 'd' }.b_as_mut(), None);
    assert_eq!(Inner::D { a: 0, b: 'd' }.c_as_mut(), None);
}