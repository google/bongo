extern crate proc_macro;

mod enum_derive;
mod struct_derive;

use {
  proc_macro::TokenStream as ProcTokenStream,
  proc_macro2::TokenStream,
  quote::{quote, ToTokens},
  syn::{
    parse::{Parse, ParseStream, Parser},
    punctuated::Punctuated,
    spanned::Spanned,
    GenericParam, Generics, Ident, Item, Result, Token, WherePredicate,
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

struct GenericsData {
  generics_clause: TokenStream,
  args_clause: TokenStream,
  where_clause: TokenStream,
}

impl GenericsData {
  pub fn new(gen: &Generics, bounds: &Vec<WherePredicate>) -> Self {
    let generics_clause;
    let args_clause;
    if gen.lt_token.is_some() {
      let params = &gen.params;
      let type_vars = params.iter().map(generic_param_literal);
      generics_clause = quote! { < #params > };
      args_clause = quote! { < #(#type_vars),* >};
    } else {
      generics_clause = quote! {};
      args_clause = quote! {};
    };

    let mut where_params = Vec::new();
    where_params
      .extend(gen.where_clause.iter().flat_map(|c| c.predicates.iter()));
    where_params.extend(bounds.iter());
    let where_clause = if where_params.is_empty() {
      quote! {}
    } else {
      quote! { where #(#where_params),* }
    };

    GenericsData {
      generics_clause,
      args_clause,
      where_clause,
    }
  }

  pub fn impl_item(
    &self,
    name: impl ToTokens,
    impl_trait: impl ToTokens,
    body: impl ToTokens,
  ) -> TokenStream {
    let GenericsData {
      generics_clause,
      args_clause,
      where_clause,
    } = self;

    quote! {
      impl #generics_clause #impl_trait for #name #args_clause #where_clause {
        #body
      }
    }
  }
}

fn struct_clone_expr<'a>(
  struct_name: impl ToTokens,
  var: impl ToTokens,
  fields: impl IntoIterator<Item = &'a TokenStream>,
) -> TokenStream {
  let field_assignments = fields.into_iter().map(|id| {
    quote! { #id : ::std::clone::Clone::clone(&#var.#id) }
  });

  quote! {
    #struct_name {
      #(#field_assignments),*
    }
  }
}

struct AttrBounds {
  where_literal: syn::LitStr,
}

impl Parse for AttrBounds {
  fn parse(input: ParseStream) -> Result<Self> {
    let where_literal: syn::LitStr = input.parse()?;
    let _: Token![,] = input.parse()?;
    Ok(AttrBounds { where_literal })
  }
}

struct AttrContents {
  where_clauses: Vec<WherePredicate>,
  traits: Punctuated<Ident, Token![,]>,
}

impl Parse for AttrContents {
  fn parse(input: ParseStream) -> Result<Self> {
    let la = input.lookahead1();
    let where_clauses = if la.peek(syn::LitStr) {
      let bounds = input.parse::<AttrBounds>()?;
      (|ps: ParseStream| -> Result<Vec<WherePredicate>> {
        let clauses: Punctuated<WherePredicate, Token![,]> =
          Punctuated::parse_terminated(ps)?;
        Ok(clauses.into_iter().collect::<Vec<_>>())
      })
      .parse_str(&*bounds.where_literal.value())?
    } else {
      Vec::new()
    };

    Ok(AttrContents {
      where_clauses,
      traits: Punctuated::parse_terminated(input)?,
    })
  }
}

fn derive_from_struct(
  attr: &AttrContents,
  st: &syn::ItemStruct,
) -> syn::Result<proc_macro2::TokenStream> {
  let impl_data = struct_derive::ImplData::new(st, &attr.where_clauses);
  let mut impl_items = Vec::new();
  for trait_id in &attr.traits {
    impl_items.push(match &*trait_id.to_string() {
      "Copy" => struct_derive::derive_copy(&impl_data),
      "Clone" => struct_derive::derive_clone(&impl_data),
      "PartialEq" => struct_derive::derive_partial_eq(&impl_data),
      "Eq" => struct_derive::derive_eq(&impl_data),
      "PartialOrd" => struct_derive::derive_partial_ord(&impl_data),
      "Ord" => struct_derive::derive_ord(&impl_data),
      "Debug" => struct_derive::derive_debug(&impl_data),
      _ => return Err(syn::Error::new_spanned(trait_id, "Unknown impl type")),
    })
  }
  Ok(quote! { #(#impl_items)* })
}

fn derive_from_enum(
  attr: &AttrContents,
  en: &syn::ItemEnum,
) -> syn::Result<proc_macro2::TokenStream> {
  let impl_data = enum_derive::ImplData::new(en, &attr.where_clauses);
  let mut impl_items = Vec::new();
  for trait_id in &attr.traits {
    impl_items.push(match &*trait_id.to_string() {
      "Copy" => enum_derive::derive_copy(&impl_data),
      "Clone" => enum_derive::derive_clone(&impl_data),
      "PartialEq" => enum_derive::derive_partial_eq(&impl_data),
      "Eq" => enum_derive::derive_eq(&impl_data),
      "PartialOrd" => enum_derive::derive_partial_ord(&impl_data),
      "Ord" => enum_derive::derive_ord(&impl_data),
      "Debug" => enum_derive::derive_debug(&impl_data),
      _ => return Err(syn::Error::new_spanned(trait_id, "Unknown impl type")),
    })
  }
  Ok(quote! { #(#impl_items)* })
}

#[proc_macro_attribute]
pub fn derive_unbounded(
  attr: ProcTokenStream,
  item: ProcTokenStream,
) -> ProcTokenStream {
  let item2 = item.clone();
  let attr_contents = syn::parse_macro_input!(attr as AttrContents);
  let parsed_item = syn::parse_macro_input!(item2 as Item);

  let result = match &parsed_item {
    Item::Struct(st) => derive_from_struct(&attr_contents, st),
    Item::Enum(en) => derive_from_enum(&attr_contents, en),
    _ => Err(syn::Error::new(
      parsed_item.span(),
      "Can't handle this kind of item.",
    )),
  };

  match result {
    Ok(ts) => (quote! {
      #parsed_item
      #ts
    })
    .into(),
    Err(e) => e.to_compile_error().into(),
  }
}

#[cfg(test)]
mod tests {
  #[test]
  fn it_works() {
    assert_eq!(2 + 2, 4);
  }
}
