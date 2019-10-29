extern crate proc_macro;

use {
  proc_macro::TokenStream,
  syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token, Ident, Result, Token, Type, Item,
  },
  quote::quote,
};

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

fn derive_from_struct(attr: &AttrContents, st: &syn::ItemStruct) -> syn::Result<proc_macro2::TokenStream> {
  unimplemented!();
}

fn derive_from_enum(attr: &AttrContents, en: &syn::ItemEnum) -> syn::Result<proc_macro2::TokenStream> {
  unimplemented!();
}

#[proc_macro_attribute]
pub fn derive_unbounded(attr: TokenStream, item: TokenStream) -> TokenStream {
  let item2 = item.clone();
  let attr_contents = syn::parse_macro_input!(attr as AttrContents);
  let parsed_item = syn::parse_macro_input!(item2 as Item);

  let result = match &parsed_item {
    Item::Struct(st) => derive_from_struct(&attr_contents, st),
    Item::Enum(en) => derive_from_enum(&attr_contents, en),
    _ => Err(syn::Error::new(parsed_item.span())),
  };

  match result {
    Ok(ts) => ts.into(),
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
