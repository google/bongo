use {
  crate::struct_clone_expr,
  proc_macro2::{Span, TokenStream},
  quote::{quote, ToTokens},
  syn::{
    Fields, GenericParam, Ident, Index, ItemStruct, LitStr, WherePredicate,
  },
};

fn generic_param_literal(param: &GenericParam) -> TokenStream {
  match param {
    GenericParam::Type(t) => {
      let id = &t.ident;
      quote! { #id }
    }
    GenericParam::Lifetime(l) => {
      quote! { #l }
    }
    GenericParam::Const(c) => {
      let id = &c.ident;
      quote! { #id }
    }
  }
}

pub struct ImplData {
  name: Ident,
  generics_clause: TokenStream,
  args_clause: TokenStream,
  where_clause: TokenStream,
  arg_names: Vec<TokenStream>,
}

impl ImplData {
  pub fn impl_item(
    &self,
    impl_trait: impl ToTokens,
    body: impl ToTokens,
  ) -> TokenStream {
    let ImplData {
      name,
      generics_clause,
      args_clause,
      where_clause,
      ..
    } = self;
    quote! {
      impl #generics_clause #impl_trait for #name #args_clause #where_clause {
        #body
      }
    }
  }
}

pub fn extract_impl_data(
  st: &ItemStruct,
  bounds: &Vec<WherePredicate>,
) -> ImplData {
  let generics = &st.generics;

  let generics_clause;
  let args_clause;
  if generics.lt_token.is_some() {
    let params = &generics.params;
    let type_vars = params.iter().map(generic_param_literal);
    generics_clause = quote! { < #params > };
    args_clause = quote! { < #(#type_vars),* >};
  } else {
    generics_clause = quote! {};
    args_clause = quote! {};
  };

  let mut where_params = Vec::new();
  where_params.extend(
    generics
      .where_clause
      .iter()
      .flat_map(|c| c.predicates.iter()),
  );
  where_params.extend(bounds.iter());
  let where_clause = if where_params.is_empty() {
    quote! {}
  } else {
    quote! { where #(#where_params),* }
  };

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
    generics_clause,
    args_clause,
    where_clause,
    arg_names,
  }
}

#[allow(dead_code)]
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

#[allow(dead_code)]
pub fn derive_copy(impl_data: &ImplData) -> TokenStream {
  impl_data.impl_item(quote! {::std::marker::Copy}, quote! {})
}

#[allow(dead_code)]
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

#[allow(dead_code)]
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

#[allow(dead_code)]
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

#[allow(dead_code)]
pub fn derive_eq(impl_data: &ImplData) -> TokenStream {
  impl_data.impl_item(quote! { ::std::cmp::Eq }, quote! {})
}

#[allow(dead_code)]
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
