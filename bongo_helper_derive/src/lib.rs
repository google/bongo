extern crate proc_macro;

mod enum_derive;
mod struct_derive;

use {
  proc_macro::TokenStream as ProcTokenStream,
  proc_macro2::TokenStream,
  quote::{quote, ToTokens},
  syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    spanned::Spanned,
    token, Ident, Item, Result, Token, Type,
  },
};

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
  comma: Token![,],
}

impl Parse for AttrBounds {
  fn parse(input: ParseStream) -> Result<Self> {
    Ok(AttrBounds {
      where_literal: input.parse()?,
      comma: input.parse()?,
    })
  }
}

struct AttrContents {
  where_literal: Option<AttrBounds>,
  traits: Punctuated<Ident, Token![,]>,
}

impl Parse for AttrContents {
  fn parse(input: ParseStream) -> Result<Self> {
    let la = input.lookahead1();
    let bounds_str;
    if la.peek(syn::LitStr) {
      bounds_str = Some(input.parse()?);
    } else {
      bounds_str = None;
    }

    Ok(AttrContents {
      where_literal: bounds_str,
      traits: Punctuated::parse_terminated(input)?,
    })
  }
}

fn derive_from_struct(
  attr: &AttrContents,
  st: &syn::ItemStruct,
) -> syn::Result<proc_macro2::TokenStream> {
  Ok(quote! {})
}

fn derive_from_enum(
  attr: &AttrContents,
  en: &syn::ItemEnum,
) -> syn::Result<proc_macro2::TokenStream> {
  unimplemented!();
}

#[proc_macro_attribute]
pub fn derive_unbounded(attr: ProcTokenStream, item: ProcTokenStream) -> ProcTokenStream {
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
