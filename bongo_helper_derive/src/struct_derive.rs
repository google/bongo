use {
  crate::{struct_clone_expr, GenericsData},
  proc_macro2::{Span, TokenStream},
  quote::{quote, ToTokens},
  syn::{Fields, Ident, Index, ItemStruct, LitStr, WherePredicate},
};

pub struct ImplData {
  name: Ident,
  generics: GenericsData,
  arg_names: Vec<TokenStream>,
}

impl ImplData {
  pub fn new(st: &ItemStruct, bounds: &Vec<WherePredicate>) -> Self {
    let generics_data = GenericsData::new(&st.generics, bounds);

    let arg_names = match &st.fields {
      Fields::Named(fields) => fields
        .named
        .iter()
        .map(|f| {
          let id = f.ident.as_ref().unwrap();
          quote! { #id }
        })
        .collect(),
      Fields::Unnamed(fields) => (0..fields.unnamed.len())
        .into_iter()
        .map(|i| {
          let index: Index = i.into();
          quote! { #index }
        })
        .collect(),
      Fields::Unit => Vec::new(),
    };

    ImplData {
      name: st.ident.clone(),
      generics: generics_data,
      arg_names,
    }
  }

  pub fn impl_item(
    &self,
    impl_trait: impl ToTokens,
    body: impl ToTokens,
  ) -> TokenStream {
    self.generics.impl_item(&self.name, impl_trait, body)
  }
}

pub fn derive_clone(impl_data: &ImplData) -> TokenStream {
  let clone_expr =
    struct_clone_expr(&impl_data.name, &quote! {self}, &impl_data.arg_names);

  impl_data.impl_item(
    quote! {::std::clone::Clone},
    quote! {
      fn clone(&self) -> Self {
        #clone_expr
      }
    },
  )
}

pub fn derive_copy(impl_data: &ImplData) -> TokenStream {
  impl_data.impl_item(quote! {::std::marker::Copy}, quote! {})
}

pub fn derive_ord(impl_data: &ImplData) -> TokenStream {
  let field_cmps = impl_data.arg_names.iter().map(|id| {
    quote! { .then_with(|| self.#id.cmp(&other.#id)) }
  });

  impl_data.impl_item(
    quote! { ::std::cmp::Ord },
    quote! {
      fn cmp(&self, other: &Self) -> ::std::cmp::Ordering {
        ::std::cmp::Ordering::Equal
        #(#field_cmps)*
      }
    },
  )
}

pub fn derive_partial_ord(impl_data: &ImplData) -> TokenStream {
  let field_cmps = impl_data.arg_names.iter().map(|id| {
    quote! { .and_then(|ord| match self.#id.partial_cmp(&other.#id) {
      Some(new_ord) => Some(ord.then(new_ord)),
      None => None,
    }) }
  });

  impl_data.impl_item(
    quote! { ::std::cmp::PartialOrd },
    quote! {
      fn partial_cmp(&self, other: &Self) -> ::std::option::Option<::std::cmp::Ordering> {
        Some(::std::cmp::Ordering::Equal)
        #(#field_cmps)*
      }
    },
  )
}

pub fn derive_partial_eq(impl_data: &ImplData) -> TokenStream {
  let field_cmps = impl_data.arg_names.iter().map(|id| {
    quote! { self.#id == other.#id }
  });

  impl_data.impl_item(
    quote! { ::std::cmp::PartialEq },
    quote! {
      fn eq(&self, other: &Self) -> bool {
        #(#field_cmps)&&*
      }
    },
  )
}

pub fn derive_eq(impl_data: &ImplData) -> TokenStream {
  impl_data.impl_item(quote! { ::std::cmp::Eq }, quote! {})
}

pub fn derive_debug(impl_data: &ImplData) -> TokenStream {
  let ImplData {
    name, arg_names, ..
  } = &impl_data;

  let struct_string_lit = LitStr::new(&*name.to_string(), name.span());

  let field_calls = arg_names.iter().map(|id| {
    let field_string_lit = LitStr::new(&*id.to_string(), Span::call_site());
    quote! { .field(#field_string_lit, &self.#id) }
  });

  impl_data.impl_item(
    quote! { ::std::fmt::Debug },
    quote! {
      fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        f.debug_struct(#struct_string_lit)
        #(#field_calls)*
        .finish()
      }
    },
  )
}
