use std::{any::Any, borrow::Borrow, collections::BTreeMap};

/// `TypeKey` is a trait whose implementing types can be used as keys to a `TypeMap`. As such, they must be
/// orderable, as well as implementing `Any`.
///
/// Each implementation has a type that it can return from a type map.
pub trait TypeMapKey: Ord + Any + 'static {
  /// The type that this key returns from a type map.
  type ValueType: Any + 'static;
}

/// Private object-safe trait to allow us to wrap instances of `TypeKey` in a type for use in a `TypeMap`.
trait TypeKeyObjectTrait: Any + 'static {
  /// Returns the current object as a dynamic reference to `Any`.
  fn as_any(&self) -> &(dyn Any + 'static);

  /// Compares this object with another object of the given type. Sorts by TypeId first, then by
  /// value after a down-cast if the type is the same.
  fn cmp_concrete(&self, other: &dyn TypeKeyObjectTrait) -> std::cmp::Ordering;
}

impl<T> TypeKeyObjectTrait for T
where
  T: TypeMapKey,
{
  fn as_any(&self) -> &(dyn Any + 'static) {
    self
  }

  fn cmp_concrete(&self, other: &dyn TypeKeyObjectTrait) -> std::cmp::Ordering {
    let this_type = std::any::TypeId::of::<T>();
    let other_type = other.as_any().type_id();

    // Compare by type first. If the types are equal, then compare by value.
    match this_type.cmp(&other_type) {
      std::cmp::Ordering::Equal => {
        let other_value = other.as_any().downcast_ref::<T>().unwrap();

        self.cmp(other_value)
      }
      other => other,
    }
  }
}

impl Ord for dyn TypeKeyObjectTrait {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    self.cmp_concrete(other)
  }
}

impl PartialOrd for dyn TypeKeyObjectTrait {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    Some(self.cmp_concrete(other))
  }
}

impl PartialEq for dyn TypeKeyObjectTrait {
  fn eq(&self, other: &Self) -> bool {
    self.cmp_concrete(other) == std::cmp::Ordering::Equal
  }
}

impl Eq for dyn TypeKeyObjectTrait {}

struct TypeKeyObject(Box<dyn TypeKeyObjectTrait + 'static>);

impl Ord for TypeKeyObject {
  fn cmp(&self, other: &Self) -> std::cmp::Ordering {
    (*self.0).cmp(&*other.0)
  }
}

impl PartialOrd for TypeKeyObject {
  fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
    (*self.0).partial_cmp(&*other.0)
  }
}

impl PartialEq for TypeKeyObject {
  fn eq(&self, other: &Self) -> bool {
    (*self.0).eq(&*other.0)
  }
}

impl Eq for TypeKeyObject {}

// We need an impl of Borrow to allow us to use references to the underlying type as lookup keys.
impl Borrow<dyn TypeKeyObjectTrait> for TypeKeyObject {
  fn borrow(&self) -> &dyn TypeKeyObjectTrait {
    &*self.0
  }
}

/// `TypeMap` is a map from keys of types that implement `TypeKey` to values. It is a heterogeneous
/// map, meaning both the types and the keys can be of different concrete types. The `TypeKey` trait is used to provide a type-safe interface on the map.
pub struct TypeMap(BTreeMap<TypeKeyObject, Box<dyn Any + 'static>>);

impl TypeMap {
  /// Creates a new empty `TypeMap`.
  pub fn new() -> Self {
    TypeMap(BTreeMap::new())
  }

  /// Inserts a new key-value pair into the map.
  pub fn insert<T: TypeMapKey>(&mut self, key: T, value: T::ValueType) {
    self.0.insert(TypeKeyObject(Box::new(key)), Box::new(value));
  }

  /// Returns a reference to the value associated with the given key.
  pub fn get<T: TypeMapKey>(&self, key: &T) -> Option<&T::ValueType> {
    let object_ref: &dyn TypeKeyObjectTrait = key;
    self
      .0
      .get(object_ref)
      .map(|v| v.downcast_ref::<T::ValueType>().unwrap())
  }

  /// Returns a mutable reference to the value associated with the given key.
  pub fn get_mut<T: TypeMapKey>(&mut self, key: &T) -> Option<&mut T::ValueType> {
    let object_ref: &dyn TypeKeyObjectTrait = key;
    self
      .0
      .get_mut(object_ref)
      .map(|v| v.downcast_mut::<T::ValueType>().unwrap())
  }

  /// Moves all of the values in the given map into this map, leaving the given map empty.
  pub fn merge(&mut self, other: &mut TypeMap) {
    self.0.append(&mut other.0);
  }
}
