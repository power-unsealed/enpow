mod outer {
    use enpow::enpow;

    #[enpow(All)]
    #[inner(type_names=Var, derive(Debug, PartialEq))]
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

use std::panic::catch_unwind;

use outer::*;

#[test]
#[rustfmt::skip]
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
    assert_eq!(Inner::D { a: 0, b: 'd' }.d(), Some(D { a: 0, b: 'd' }));
    assert_eq!(Inner::D { a: 0, b: 'd' }.a(), None);
    assert_eq!(Inner::D { a: 0, b: 'd' }.b(), None);
    assert_eq!(Inner::D { a: 0, b: 'd' }.c(), None);
}

#[test]
#[rustfmt::skip]
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
#[rustfmt::skip]
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
    assert_eq!(Inner::D { a: 0, b: 'd' }.d_as_ref(), Some(DRef { a: &0, b: &'d' }));
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
    assert_eq!(Inner::D { a: 0, b: 'd' }.d_as_mut(), Some(DMut { a: &mut 0, b: &mut 'd' }));
    assert_eq!(Inner::D { a: 0, b: 'd' }.a_as_mut(), None);
    assert_eq!(Inner::D { a: 0, b: 'd' }.b_as_mut(), None);
    assert_eq!(Inner::D { a: 0, b: 'd' }.c_as_mut(), None);
}

#[test]
#[rustfmt::skip]
fn map_variant() {
    assert_eq!(Inner::<i32, char>::A.map_a_or((), |a| a), ());
    assert_eq!(Inner::<i32, char>::A.map_b_or(0, |_| unreachable!()), 0);
    assert_eq!(Inner::<i32, char>::A.map_c_or((0, 'c'), |_| unreachable!()), (0, 'c'));
    assert_eq!(Inner::<i32, char>::A.map_d_or(D { a: 0, b: 'd' }, |_| unreachable!()), D { a: 0, b: 'd' });
    assert_eq!(Inner::<i32, char>::B(0).map_b_or(1, |b| b), 0);
    assert_eq!(Inner::<i32, char>::B(0).map_c_or((0, 'c'), |_| unreachable!()), (0, 'c'));
    assert_eq!(Inner::<i32, char>::B(0).map_d_or(D { a: 0, b: 'd' }, |_| unreachable!()), D { a: 0, b: 'd' });
    assert_eq!(Inner::<i32, char>::B(0).map_a_or((), |_| unreachable!()), ());
    assert_eq!(Inner::<i32, char>::C(0, 'c').map_c_or((1, 'c'), |c| c), (0, 'c'));
    assert_eq!(Inner::<i32, char>::C(0, 'c').map_d_or(D { a: 0, b: 'd' }, |_| unreachable!()), D { a: 0, b: 'd' });
    assert_eq!(Inner::<i32, char>::C(0, 'c').map_a_or((), |_| unreachable!()), ());
    assert_eq!(Inner::<i32, char>::C(0, 'c').map_b_or(0, |_| unreachable!()), 0);
    assert_eq!(Inner::D { a: 0, b: 'd' }.map_d_or(D { a: 1, b: 'd' }, |d| d), D { a: 0, b: 'd' });
    assert_eq!(Inner::D { a: 0, b: 'd' }.map_a_or((), |_| unreachable!()), ());
    assert_eq!(Inner::D { a: 0, b: 'd' }.map_b_or(0, |_| unreachable!()), 0);
    assert_eq!(Inner::D { a: 0, b: 'd' }.map_c_or((0, 'c'), |_| unreachable!()), (0, 'c'));
    
    assert_eq!(Inner::<i32, char>::A.map_a_or_else(|_| unreachable!(), |a| a), ());
    assert_eq!(Inner::<i32, char>::A.map_b_or_else(|_| 0, |_| unreachable!()), 0);
    assert_eq!(Inner::<i32, char>::A.map_c_or_else(|_| (0, 'c'), |_| unreachable!()), (0, 'c'));
    assert_eq!(Inner::<i32, char>::A.map_d_or_else(|_| D { a: 0, b: 'd' }, |_| unreachable!()), D { a: 0, b: 'd' });
    assert_eq!(Inner::<i32, char>::B(0).map_b_or_else(|_| unreachable!(), |b| b), 0);
    assert_eq!(Inner::<i32, char>::B(0).map_c_or_else(|_| (0, 'c'), |_| unreachable!()), (0, 'c'));
    assert_eq!(Inner::<i32, char>::B(0).map_d_or_else(|_| D { a: 0, b: 'd' }, |_| unreachable!()), D { a: 0, b: 'd' });
    assert_eq!(Inner::<i32, char>::B(0).map_a_or_else(|_| (), |_| unreachable!()), ());
    assert_eq!(Inner::<i32, char>::C(0, 'c').map_c_or_else(|_| unreachable!(), |c| c), (0, 'c'));
    assert_eq!(Inner::<i32, char>::C(0, 'c').map_d_or_else(|_| D { a: 0, b: 'd' }, |_| unreachable!()), D { a: 0, b: 'd' });
    assert_eq!(Inner::<i32, char>::C(0, 'c').map_a_or_else(|_| (), |_| unreachable!()), ());
    assert_eq!(Inner::<i32, char>::C(0, 'c').map_b_or_else(|_| 0, |_| unreachable!()), 0);
    assert_eq!(Inner::D { a: 0, b: 'd' }.map_d_or_else(|_| unreachable!(), |d| d), D { a: 0, b: 'd' });
    assert_eq!(Inner::D { a: 0, b: 'd' }.map_a_or_else(|_| (), |_| unreachable!()), ());
    assert_eq!(Inner::D { a: 0, b: 'd' }.map_b_or_else(|_| 0, |_| unreachable!()), 0);
    assert_eq!(Inner::D { a: 0, b: 'd' }.map_c_or_else(|_| (0, 'c'), |_| unreachable!()), (0, 'c'));
}

#[test]
#[rustfmt::skip]
fn unwrap_variant() {
    Inner::<i32, char>::A.unwrap_a();
    catch_unwind(|| Inner::<i32, char>::A.unwrap_b()).unwrap_err();
    catch_unwind(|| Inner::<i32, char>::A.unwrap_c()).unwrap_err();
    catch_unwind(|| Inner::<i32, char>::A.unwrap_d()).unwrap_err();
    Inner::<i32, char>::B(0).unwrap_b();
    catch_unwind(|| Inner::<i32, char>::B(0).unwrap_c()).unwrap_err();
    catch_unwind(|| Inner::<i32, char>::B(0).unwrap_d()).unwrap_err();
    catch_unwind(|| Inner::<i32, char>::B(0).unwrap_a()).unwrap_err();
    Inner::<i32, char>::C(0, 'c').unwrap_c();
    catch_unwind(|| Inner::<i32, char>::C(0, 'c').unwrap_d()).unwrap_err();
    catch_unwind(|| Inner::<i32, char>::C(0, 'c').unwrap_a()).unwrap_err();
    catch_unwind(|| Inner::<i32, char>::C(0, 'c').unwrap_b()).unwrap_err();
    Inner::D { a: 0, b: 'd' }.unwrap_d();
    catch_unwind(|| Inner::D { a: 0, b: 'd' }.unwrap_a()).unwrap_err();
    catch_unwind(|| Inner::D { a: 0, b: 'd' }.unwrap_b()).unwrap_err();
    catch_unwind(|| Inner::D { a: 0, b: 'd' }.unwrap_c()).unwrap_err();

    Inner::<i32, char>::A.unwrap_a_as_ref();
    catch_unwind(|| Inner::<i32, char>::A.unwrap_b_as_ref()).unwrap_err();
    catch_unwind(|| Inner::<i32, char>::A.unwrap_c_as_ref()).unwrap_err();
    catch_unwind(|| Inner::<i32, char>::A.unwrap_d_as_ref()).unwrap_err();
    Inner::<i32, char>::B(0).unwrap_b_as_ref();
    catch_unwind(|| Inner::<i32, char>::B(0).unwrap_c_as_ref()).unwrap_err();
    catch_unwind(|| Inner::<i32, char>::B(0).unwrap_d_as_ref()).unwrap_err();
    catch_unwind(|| Inner::<i32, char>::B(0).unwrap_a_as_ref()).unwrap_err();
    Inner::<i32, char>::C(0, 'c').unwrap_c_as_ref();
    catch_unwind(|| Inner::<i32, char>::C(0, 'c').unwrap_d_as_ref()).unwrap_err();
    catch_unwind(|| Inner::<i32, char>::C(0, 'c').unwrap_a_as_ref()).unwrap_err();
    catch_unwind(|| Inner::<i32, char>::C(0, 'c').unwrap_b_as_ref()).unwrap_err();
    Inner::D { a: 0, b: 'd' }.unwrap_d_as_ref();
    catch_unwind(|| Inner::D { a: 0, b: 'd' }.unwrap_a_as_ref()).unwrap_err();
    catch_unwind(|| Inner::D { a: 0, b: 'd' }.unwrap_b_as_ref()).unwrap_err();
    catch_unwind(|| Inner::D { a: 0, b: 'd' }.unwrap_c_as_ref()).unwrap_err();

    Inner::<i32, char>::A.unwrap_a_as_mut();
    catch_unwind(|| { Inner::<i32, char>::A.unwrap_b_as_mut(); }).unwrap_err();
    catch_unwind(|| { Inner::<i32, char>::A.unwrap_c_as_mut(); }).unwrap_err();
    catch_unwind(|| { Inner::<i32, char>::A.unwrap_d_as_mut(); }).unwrap_err();
    Inner::<i32, char>::B(0).unwrap_b_as_mut();
    catch_unwind(|| { Inner::<i32, char>::B(0).unwrap_c_as_mut(); }).unwrap_err();
    catch_unwind(|| { Inner::<i32, char>::B(0).unwrap_d_as_mut(); }).unwrap_err();
    catch_unwind(|| { Inner::<i32, char>::B(0).unwrap_a_as_mut(); }).unwrap_err();
    Inner::<i32, char>::C(0, 'c').unwrap_c_as_mut();
    catch_unwind(|| { Inner::<i32, char>::C(0, 'c').unwrap_d_as_mut(); }).unwrap_err();
    catch_unwind(|| { Inner::<i32, char>::C(0, 'c').unwrap_a_as_mut(); }).unwrap_err();
    catch_unwind(|| { Inner::<i32, char>::C(0, 'c').unwrap_b_as_mut(); }).unwrap_err();
    Inner::D { a: 0, b: 'd' }.unwrap_d_as_mut();
    catch_unwind(|| { Inner::D { a: 0, b: 'd' }.unwrap_a_as_mut(); }).unwrap_err();
    catch_unwind(|| { Inner::D { a: 0, b: 'd' }.unwrap_b_as_mut(); }).unwrap_err();
    catch_unwind(|| { Inner::D { a: 0, b: 'd' }.unwrap_c_as_mut(); }).unwrap_err();

    assert_eq!(Inner::<i32, char>::A.unwrap_a_or(()), ());
    assert_eq!(Inner::<i32, char>::A.unwrap_b_or(0), 0);
    assert_eq!(Inner::<i32, char>::A.unwrap_c_or((0, 'c')), (0, 'c'));
    assert_eq!(Inner::<i32, char>::A.unwrap_d_or(D { a: 0, b: 'd' }), D { a: 0, b: 'd' });
    assert_eq!(Inner::<i32, char>::B(0).unwrap_b_or(1), 0);
    assert_eq!(Inner::<i32, char>::B(0).unwrap_c_or((0, 'c')), (0, 'c'));
    assert_eq!(Inner::<i32, char>::B(0).unwrap_d_or(D { a: 0, b: 'd' }), D { a: 0, b: 'd' });
    assert_eq!(Inner::<i32, char>::B(0).unwrap_a_or(()), ());
    assert_eq!(Inner::<i32, char>::C(0, 'c').unwrap_c_or((1, 'c')), (0, 'c'));
    assert_eq!(Inner::<i32, char>::C(0, 'c').unwrap_d_or(D { a: 0, b: 'd' }), D { a: 0, b: 'd' });
    assert_eq!(Inner::<i32, char>::C(0, 'c').unwrap_a_or(()), ());
    assert_eq!(Inner::<i32, char>::C(0, 'c').unwrap_b_or(0), 0);
    assert_eq!(Inner::D { a: 0, b: 'd' }.unwrap_d_or(D { a: 1, b: 'd' }), D { a: 0, b: 'd' });
    assert_eq!(Inner::D { a: 0, b: 'd' }.unwrap_a_or(()), ());
    assert_eq!(Inner::D { a: 0, b: 'd' }.unwrap_b_or(0), 0);
    assert_eq!(Inner::D { a: 0, b: 'd' }.unwrap_c_or((0, 'c')), (0, 'c'));

    assert_eq!(Inner::<i32, char>::A.unwrap_a_or_else(|_| ()), ());
    assert_eq!(Inner::<i32, char>::A.unwrap_b_or_else(|_| 0), 0);
    assert_eq!(Inner::<i32, char>::A.unwrap_c_or_else(|_| (0, 'c')), (0, 'c'));
    assert_eq!(Inner::<i32, char>::A.unwrap_d_or_else(|_| D { a: 0, b: 'd' }), D { a: 0, b: 'd' });
    assert_eq!(Inner::<i32, char>::B(0).unwrap_b_or_else(|_| 1), 0);
    assert_eq!(Inner::<i32, char>::B(0).unwrap_c_or_else(|_| (0, 'c')), (0, 'c'));
    assert_eq!(Inner::<i32, char>::B(0).unwrap_d_or_else(|_| D { a: 0, b: 'd' }), D { a: 0, b: 'd' });
    assert_eq!(Inner::<i32, char>::B(0).unwrap_a_or_else(|_| ()), ());
    assert_eq!(Inner::<i32, char>::C(0, 'c').unwrap_c_or_else(|_| (1, 'c')), (0, 'c'));
    assert_eq!(Inner::<i32, char>::C(0, 'c').unwrap_d_or_else(|_| D { a: 0, b: 'd' }), D { a: 0, b: 'd' });
    assert_eq!(Inner::<i32, char>::C(0, 'c').unwrap_a_or_else(|_| ()), ());
    assert_eq!(Inner::<i32, char>::C(0, 'c').unwrap_b_or_else(|_| 0), 0);
    assert_eq!(Inner::D { a: 0, b: 'd' }.unwrap_d_or_else(|_| D { a: 1, b: 'd' }), D { a: 0, b: 'd' });
    assert_eq!(Inner::D { a: 0, b: 'd' }.unwrap_a_or_else(|_| ()), ());
    assert_eq!(Inner::D { a: 0, b: 'd' }.unwrap_b_or_else(|_| 0), 0);
    assert_eq!(Inner::D { a: 0, b: 'd' }.unwrap_c_or_else(|_| (0, 'c')), (0, 'c'));
}

#[test]
#[rustfmt::skip]
fn expect_variant() {
    Inner::<i32, char>::A.expect_a("");
    catch_unwind(|| Inner::<i32, char>::A.expect_b("")).unwrap_err();
    catch_unwind(|| Inner::<i32, char>::A.expect_c("")).unwrap_err();
    catch_unwind(|| Inner::<i32, char>::A.expect_d("")).unwrap_err();
    Inner::<i32, char>::B(0).expect_b("");
    catch_unwind(|| Inner::<i32, char>::B(0).expect_c("")).unwrap_err();
    catch_unwind(|| Inner::<i32, char>::B(0).expect_d("")).unwrap_err();
    catch_unwind(|| Inner::<i32, char>::B(0).expect_a("")).unwrap_err();
    Inner::<i32, char>::C(0, 'c').expect_c("");
    catch_unwind(|| Inner::<i32, char>::C(0, 'c').expect_d("")).unwrap_err();
    catch_unwind(|| Inner::<i32, char>::C(0, 'c').expect_a("")).unwrap_err();
    catch_unwind(|| Inner::<i32, char>::C(0, 'c').expect_b("")).unwrap_err();
    Inner::D { a: 0, b: 'd' }.expect_d("");
    catch_unwind(|| Inner::D { a: 0, b: 'd' }.expect_a("")).unwrap_err();
    catch_unwind(|| Inner::D { a: 0, b: 'd' }.expect_b("")).unwrap_err();
    catch_unwind(|| Inner::D { a: 0, b: 'd' }.expect_c("")).unwrap_err();
    
    Inner::<i32, char>::A.expect_a_as_ref("");
    catch_unwind(|| Inner::<i32, char>::A.expect_b_as_ref("")).unwrap_err();
    catch_unwind(|| Inner::<i32, char>::A.expect_c_as_ref("")).unwrap_err();
    catch_unwind(|| Inner::<i32, char>::A.expect_d_as_ref("")).unwrap_err();
    Inner::<i32, char>::B(0).expect_b_as_ref("");
    catch_unwind(|| Inner::<i32, char>::B(0).expect_c_as_ref("")).unwrap_err();
    catch_unwind(|| Inner::<i32, char>::B(0).expect_d_as_ref("")).unwrap_err();
    catch_unwind(|| Inner::<i32, char>::B(0).expect_a_as_ref("")).unwrap_err();
    Inner::<i32, char>::C(0, 'c').expect_c_as_ref("");
    catch_unwind(|| Inner::<i32, char>::C(0, 'c').expect_d_as_ref("")).unwrap_err();
    catch_unwind(|| Inner::<i32, char>::C(0, 'c').expect_a_as_ref("")).unwrap_err();
    catch_unwind(|| Inner::<i32, char>::C(0, 'c').expect_b_as_ref("")).unwrap_err();
    Inner::D { a: 0, b: 'd' }.expect_d_as_ref("");
    catch_unwind(|| Inner::D { a: 0, b: 'd' }.expect_a_as_ref("")).unwrap_err();
    catch_unwind(|| Inner::D { a: 0, b: 'd' }.expect_b_as_ref("")).unwrap_err();
    catch_unwind(|| Inner::D { a: 0, b: 'd' }.expect_c_as_ref("")).unwrap_err();
    
    Inner::<i32, char>::A.expect_a_as_mut("");
    catch_unwind(|| { Inner::<i32, char>::A.expect_b_as_mut(""); }).unwrap_err();
    catch_unwind(|| { Inner::<i32, char>::A.expect_c_as_mut(""); }).unwrap_err();
    catch_unwind(|| { Inner::<i32, char>::A.expect_d_as_mut(""); }).unwrap_err();
    Inner::<i32, char>::B(0).expect_b_as_mut("");
    catch_unwind(|| { Inner::<i32, char>::B(0).expect_c_as_mut(""); }).unwrap_err();
    catch_unwind(|| { Inner::<i32, char>::B(0).expect_d_as_mut(""); }).unwrap_err();
    catch_unwind(|| { Inner::<i32, char>::B(0).expect_a_as_mut(""); }).unwrap_err();
    Inner::<i32, char>::C(0, 'c').expect_c_as_mut("");
    catch_unwind(|| { Inner::<i32, char>::C(0, 'c').expect_d_as_mut(""); }).unwrap_err();
    catch_unwind(|| { Inner::<i32, char>::C(0, 'c').expect_a_as_mut(""); }).unwrap_err();
    catch_unwind(|| { Inner::<i32, char>::C(0, 'c').expect_b_as_mut(""); }).unwrap_err();
    Inner::D { a: 0, b: 'd' }.expect_d_as_mut("");
    catch_unwind(|| { Inner::D { a: 0, b: 'd' }.expect_a_as_mut(""); }).unwrap_err();
    catch_unwind(|| { Inner::D { a: 0, b: 'd' }.expect_b_as_mut(""); }).unwrap_err();
    catch_unwind(|| { Inner::D { a: 0, b: 'd' }.expect_c_as_mut(""); }).unwrap_err();
}
