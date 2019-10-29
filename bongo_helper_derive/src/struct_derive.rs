use {
  proc_macro2::TokenStream,
  quote::quote,
  syn::{
    Fields, GenericParam, Ident, Index, ItemStruct, Result, WherePredicate,
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

struct ImplData {
  name: Ident,
  generics_clause: TokenStream,
  args_clause: TokenStream,
  where_clause: TokenStream,
  arg_names: Vec<TokenStream>,
}

fn extract_impl_data(
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
pub fn derive_clone(
  st: &ItemStruct,
  bounds: &Vec<WherePredicate>,
) -> Result<TokenStream> {
  let impl_data = extract_impl_data(st, bounds);

  let field_assignments = impl_data.arg_names.iter().map(|id| {
    quote! { #id : ::std::clone::Clone::clone(&self.#id) }
  });

  let ImplData {
    name,
    generics_clause,
    args_clause,
    where_clause,
    ..
  } = &impl_data;

  Ok(quote! {
    impl #generics_clause ::std::clone::Clone for #name #args_clause #where_clause {
      fn clone(&self) -> Self {
        #name {
          #(#field_assignments),*
        }
      }
    }
  })
}

#[allow(dead_code)]
pub fn derive_ord(
  st: &ItemStruct,
  bounds: &Vec<WherePredicate>,
) -> Result<TokenStream> {
  let impl_data = extract_impl_data(st, bounds);

  let ImplData {
    name,
    generics_clause,
    args_clause,
    where_clause,
    arg_names,
  } = &impl_data;

  let field_cmps = arg_names.iter().map(|id| {
    quote! { .then_with(|| self.#id.cmp(&other.#id)) }
  });

  Ok(quote! {
    impl #generics_clause ::std::cmp::Ord for #name #args_clause #where_clause {
      fn cmp(&self, other: &Self) -> ::std::cmp::Ordering {
        ::std::cmp::Ordering::Equal
        #(#field_cmps)*
      }
    }
  })
}

#[allow(dead_code)]
pub fn derive_partial_ord(
  st: &ItemStruct,
  bounds: &Vec<WherePredicate>,
) -> Result<TokenStream> {
  let impl_data = extract_impl_data(st, bounds);

  let ImplData {
    name,
    generics_clause,
    args_clause,
    where_clause,
    arg_names,
  } = &impl_data;

  let field_cmps = arg_names.iter().map(|id| {
    quote! { .and_then(|ord| match self.#id.cmp(&other.#id) {
      Some(new_ord) => Some(ord.then(new_ord)),
      None => None,
    }) }
  });

  Ok(quote! {
    impl #generics_clause ::std::cmp::PartialOrd for #name #args_clause #where_clause {
      fn cmp(&self, other: &Self) -> ::std::cmp::Ordering {
        Some(::std::cmp::Ordering::Equal)
        #(#field_cmps)*
      }
    }
  })
}

#[allow(dead_code)]
pub fn derive_partial_eq(
  st: &ItemStruct,
  bounds: &Vec<WherePredicate>,
) -> Result<TokenStream> {
  let impl_data = extract_impl_data(st, bounds);

  let ImplData {
    name,
    generics_clause,
    args_clause,
    where_clause,
    arg_names,
  } = &impl_data;

  let field_cmps = arg_names.iter().map(|id| {
    quote! { self.#id == other.#id }
  });

  Ok(quote! {
    impl #generics_clause ::std::cmp::PartialEq for #name #args_clause #where_clause {
      fn eq(&self, other: &Self) -> ::std::cmp::Ordering {
        #(#field_cmps)&&*
      }
    }
  })
}

#[allow(dead_code)]
pub fn derive_eq(
  st: &ItemStruct,
  bounds: &Vec<WherePredicate>,
) -> Result<TokenStream> {
  let impl_data = extract_impl_data(st, bounds);

  let ImplData {
    name,
    generics_clause,
    args_clause,
    where_clause,
    arg_names,
  } = &impl_data;

  let field_cmps = arg_names.iter().map(|id| {
    quote! { self.#id == other.#id }
  });

  Ok(quote! {
    impl #generics_clause ::std::cmp::PartialOrd for #name #args_clause #where_clause {
      fn cmp(&self, other: &Self) -> ::std::cmp::Ordering {
        #(#field_cmps)&&*
      }
    }
  })
}
