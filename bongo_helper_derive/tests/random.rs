#[bongo_helper_derive::derive_unbounded("A: B + C", Clone, Ord, PartialOrd)]
struct Foo(u32);

#[test]
fn my_test() {
  let x = Foo(3);
  let Foo(y) = x;
}