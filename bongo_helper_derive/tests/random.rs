use bongo_helper_derive::derive_unbounded;

struct Dumb<T>(::std::marker::PhantomData<T>);

impl<T> Copy for Dumb<T> {}

impl<T> Clone for Dumb<T> {
  fn clone(&self) -> Self {
    *self
  }
}

impl<T> PartialEq for Dumb<T> {
  fn eq(&self, other: &Self) -> bool {
    true
  }
}

impl<T> Eq for Dumb<T> {}

impl<T> PartialOrd for Dumb<T> {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(std::cmp::Ordering::Equal)
  }
}

impl<T> Ord for Dumb<T> {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    std::cmp::Ordering::Equal
  }
}

impl<T> std::fmt::Debug for Dumb<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    f.debug_tuple("Dumb").finish()
  }
}

#[derive_unbounded(Copy, Clone, PartialEq, Eq, Debug)]
#[derive_unbounded(Ord, PartialOrd)]
struct Foo<T>(u32, ::std::marker::PhantomData<T>);

#[derive_unbounded(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum Bar<T> {
  A(u32, Dumb<T>),
  B { n: u32 },
  C,
}

#[test]
fn my_test() {
  let x: Foo<String> = Foo(3, ::std::marker::PhantomData {});
  assert!(x == x);
  let Foo(_y, _) = x;
}
