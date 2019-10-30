use bongo_helper_derive::derive_unbounded;

#[derive_unbounded(Copy, Clone, PartialEq, Eq, Debug)]
#[derive_unbounded(Ord, PartialOrd)]
struct Foo<T>(u32, ::std::marker::PhantomData<T>);

#[test]
fn my_test() {
  let x: Foo<String> = Foo(3, ::std::marker::PhantomData {});
  assert!(x == x);
  let Foo(_y, _) = x;
}
