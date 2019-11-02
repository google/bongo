use {
  crate::GenericsData,
  proc_macro2::{Span, TokenStream},
  quote::{quote, ToTokens},
  syn::{Fields, Ident, ItemEnum, LitStr, Variant, WherePredicate},
};

pub struct ImplData {
  name: Ident,
  generics: GenericsData,
  variants: Vec<VariantImplData>,
}

impl ImplData {
  pub fn new(en: &ItemEnum, bounds: &Vec<WherePredicate>) -> Self {
    let generics = GenericsData::new(&en.generics, bounds);

    let variants = en
      .variants
      .iter()
      .map(|variant| VariantImplData::new(variant))
      .collect();

    ImplData {
      name: en.ident.clone(),
      generics,
      variants,
    }
  }

  pub fn impl_item(
    &self,
    trait_name: impl ToTokens,
    body: impl ToTokens,
  ) -> TokenStream {
    self.generics.impl_item(&self.name, trait_name, body)
  }

  fn num_variants(&self) -> usize {
    self.variants.len()
  }

  fn match_any(&self, variant_index: usize) -> TokenStream {
    self.variants[variant_index].match_any(&self.name)
  }

  fn match_vars(
    &self,
    variant_index: usize,
    prefix: &str,
  ) -> (Vec<Ident>, TokenStream) {
    self.variants[variant_index].match_vars(&self.name, prefix)
  }

  fn value_expr(
    &self,
    variant_index: usize,
    exprs: &[TokenStream],
  ) -> TokenStream {
    self.variants[variant_index].value_expr(&self.name, exprs)
  }

  fn debug_expr(
    &self,
    variant_index: usize,
    fmt_expr: impl ToTokens,
    exprs: &[TokenStream],
  ) -> TokenStream {
    self.variants[variant_index].debug_expr(
      &self.name.to_string(),
      fmt_expr,
      exprs,
    )
  }
}

struct VariantImplData {
  name: Ident,
  args: VariantArgsData,
}

impl VariantImplData {
  fn new(v: &Variant) -> Self {
    VariantImplData {
      name: v.ident.clone(),
      args: VariantArgsData::new(&v.fields),
    }
  }

  pub fn match_any(&self, enum_name: impl ToTokens) -> TokenStream {
    let var_name = &self.name;
    let body = self.args.match_any_body();
    quote! { #enum_name :: #var_name #body }
  }

  pub fn match_vars(
    &self,
    enum_name: impl ToTokens,
    prefix: &str,
  ) -> (Vec<Ident>, TokenStream) {
    let var_name = &self.name;
    let (args, body) = self.args.match_vars_body(prefix);
    let pattern = quote! { #enum_name :: #var_name #body };
    (args, pattern)
  }

  pub fn value_expr(
    &self,
    enum_name: impl ToTokens,
    exprs: &[TokenStream],
  ) -> TokenStream {
    let var_name = &self.name;
    let value_body = self.args.value_body(exprs);
    quote!(#enum_name :: #var_name #value_body)
  }

  fn debug_expr(
    &self,
    enum_name: &str,
    fmt_expr: impl ToTokens,
    exprs: &[TokenStream],
  ) -> TokenStream {
    let complete_name = format!("{}::{}", enum_name, self.name.to_string());
    self.args.debug_expr(&complete_name, fmt_expr, exprs)
  }
}

enum VariantArgsData {
  UnnamedBody { num_args: usize },
  NamedBody { fields: Vec<Ident> },
  UnitBody,
}

impl VariantArgsData {
  fn new(fields: &Fields) -> Self {
    match fields {
      Fields::Unnamed(f) => VariantArgsData::UnnamedBody {
        num_args: f.unnamed.len(),
      },
      Fields::Named(f) => VariantArgsData::NamedBody {
        fields: f
          .named
          .iter()
          .map(|field| field.ident.clone().unwrap())
          .collect(),
      },
      Fields::Unit => VariantArgsData::UnitBody,
    }
  }

  pub fn match_any_body(&self) -> TokenStream {
    match self {
      VariantArgsData::UnnamedBody { num_args } => {
        let args = (0..*num_args).into_iter().map(|_| quote!(_));
        quote!(( #(#args),* ))
      }
      VariantArgsData::NamedBody { fields } => {
        let args = fields.iter().map(|id| quote!(#id : _));
        quote!({ #(#args),* })
      }
      VariantArgsData::UnitBody => quote!(),
    }
  }

  pub fn match_vars_body(&self, prefix: &str) -> (Vec<Ident>, TokenStream) {
    match self {
      VariantArgsData::UnnamedBody { num_args } => {
        let args = (0..*num_args)
          .into_iter()
          .map(|i| {
            let mut new_ident_name = String::new();
            new_ident_name += prefix;
            new_ident_name += &i.to_string();
            Ident::new(&new_ident_name, Span::call_site())
          })
          .collect::<Vec<_>>();
        let body = quote!(( #(#args),* ));
        (args, body)
      }
      VariantArgsData::NamedBody { fields } => {
        let args = fields
          .iter()
          .map(|id| {
            let mut new_ident_name = String::new();
            new_ident_name += prefix;
            new_ident_name += &id.to_string();
            Ident::new(&new_ident_name, Span::call_site())
          })
          .collect::<Vec<_>>();
        let body = quote!({ #(#fields: #args),* });
        (args, body)
      }
      VariantArgsData::UnitBody => (Vec::new(), quote!()),
    }
  }

  pub fn value_body(&self, exprs: &[TokenStream]) -> TokenStream {
    match self {
      VariantArgsData::UnnamedBody { num_args } => {
        assert_eq!(*num_args, exprs.len());
        quote!( ( #(#exprs),* ))
      }
      VariantArgsData::NamedBody { fields } => {
        assert_eq!(fields.len(), exprs.len());
        let field_defs = fields
          .iter()
          .zip(exprs.iter())
          .map(|(field_name, expr)| quote!(#field_name: #expr));
        quote!( { #(#field_defs),* })
      }
      VariantArgsData::UnitBody => quote!(),
    }
  }

  fn debug_expr(
    &self,
    name: &str,
    fmt_expr: impl ToTokens,
    exprs: &[TokenStream],
  ) -> TokenStream {
    let name_lit = LitStr::new(name, Span::call_site());
    match self {
      VariantArgsData::UnnamedBody { num_args } => {
        assert_eq!(*num_args, exprs.len());
        let calls = exprs.iter().map(|expr| quote!(.field(#expr)));
        quote!(#fmt_expr.debug_tuple(#name_lit) #(#calls)* .finish())
      }
      VariantArgsData::NamedBody { fields } => {
        assert_eq!(fields.len(), exprs.len());
        let calls =
          fields.iter().zip(exprs.iter()).map(|(field_name, expr)| {
            let field_name_lit =
              LitStr::new(&field_name.to_string(), Span::call_site());
            quote!(.field(#field_name_lit, #expr))
          });

        quote!(#fmt_expr.debug_struct(#name_lit) #(#calls)* .finish())
      }
      VariantArgsData::UnitBody => quote!(#fmt_expr.write_str(#name_lit)),
    }
  }
}

pub fn derive_clone(impl_data: &ImplData) -> TokenStream {
  let num_variants = impl_data.num_variants();
  let mut match_cases = Vec::new();
  for i in 0..num_variants {
    let (args, pattern) = impl_data.match_vars(i, "self_");
    let exprs = args
      .iter()
      .map(|arg| quote!(#arg.clone()))
      .collect::<Vec<_>>();
    let clone_expr = impl_data.value_expr(i, &*exprs);
    match_cases.push(quote!(#pattern => #clone_expr,))
  }

  impl_data.impl_item(
    quote!(::std::clone::Clone),
    quote! {
      fn clone(&self) -> Self {
        match self {
          #(#match_cases)*
        }
      }
    },
  )
}

pub fn derive_copy(impl_data: &ImplData) -> TokenStream {
  impl_data.impl_item(quote!(::std::marker::Copy), quote!())
}

pub fn derive_partial_eq(impl_data: &ImplData) -> TokenStream {
  let mut match_clauses = Vec::new();
  for i in 0..impl_data.num_variants() {
    let (self_args, self_pattern) = impl_data.match_vars(i, "self_");
    let (other_args, other_pattern) = impl_data.match_vars(i, "other_");
    let eq_exprs = self_args
      .iter()
      .zip(other_args.iter())
      .map(|(self_arg, other_arg)| quote!(&& #self_arg == #other_arg));

    match_clauses
      .push(quote!( (#self_pattern, #other_pattern) => true #(#eq_exprs)*,));
  }
  impl_data.impl_item(
    quote!(::std::cmp::PartialEq),
    quote! {
      fn eq(&self, other: &Self) -> bool {
        match (self, other) {
          #(#match_clauses)*
          _ => false,
        }
      }
    },
  )
}

pub fn derive_eq(impl_data: &ImplData) -> TokenStream {
  impl_data.impl_item(quote!(::std::cmp::Eq), quote!())
}

pub fn derive_partial_ord(impl_data: &ImplData) -> TokenStream {
  let mut match_clauses = Vec::new();
  for i in 0..impl_data.num_variants() {
    for j in 0..impl_data.num_variants() {
      let clause = if i == j {
        let (self_args, self_pattern) = impl_data.match_vars(i, "self_");
        let (other_args, other_pattern) = impl_data.match_vars(i, "other_");

        let eq_exprs = self_args.iter().zip(other_args.iter()).map(
          |(self_arg, other_arg)| {
            quote! {
              .and_then(|ord| match #self_arg.partial_cmp(&#other_arg) {
                Some(new_ord) => Some(ord.then(new_ord)),
                None => None,
              })
            }
          },
        );

        quote! {
          (#self_pattern, #other_pattern) =>
            ::std::option::Option::Some(::std::cmp::Ordering::Equal)
              #(#eq_exprs)*,
        }
      } else {
        let self_any = impl_data.match_any(i);
        let other_any = impl_data.match_any(j);

        let expr = if i < j {
          quote!(::std::option::Option::Some(::std::cmp::Ordering::Less))
        } else {
          quote!(::std::option::Option::Some(::std::cmp::Ordering::Greater))
        };
        quote!((#self_any, #other_any) => #expr,)
      };

      match_clauses.push(clause)
    }
  }
  impl_data.impl_item(
    quote!(::std::cmp::PartialOrd),
    quote! {
      fn partial_cmp(&self, other: &Self) -> ::std::option::Option<::std::cmp::Ordering> {
        match (self, other) {
          #(#match_clauses)*
        }
      }
    },
  )
}

pub fn derive_ord(impl_data: &ImplData) -> TokenStream {
  let mut match_clauses = Vec::new();
  for i in 0..impl_data.num_variants() {
    for j in 0..impl_data.num_variants() {
      let clause = if i == j {
        let (self_args, self_pattern) = impl_data.match_vars(i, "self_");
        let (other_args, other_pattern) = impl_data.match_vars(i, "other_");

        let eq_exprs = self_args.iter().zip(other_args.iter()).map(
          |(self_arg, other_arg)| {
            quote! {
              .then_with(|| #self_arg.cmp(&#other_arg))
            }
          },
        );

        quote! {
          (#self_pattern, #other_pattern) =>
            ::std::cmp::Ordering::Equal
              #(#eq_exprs)*,
        }
      } else {
        let self_any = impl_data.match_any(i);
        let other_any = impl_data.match_any(j);

        let expr = if i < j {
          quote!(::std::cmp::Ordering::Less)
        } else {
          quote!(::std::cmp::Ordering::Greater)
        };
        quote!((#self_any, #other_any) => #expr,)
      };

      match_clauses.push(clause)
    }
  }

  impl_data.impl_item(
    quote!(::std::cmp::Ord),
    quote! {
      fn cmp(&self, other: &Self) -> ::std::cmp::Ordering {
        match (self, other) {
          #(#match_clauses)*
        }
      }
    },
  )
}

pub fn derive_debug(impl_data: &ImplData) -> TokenStream {
  let mut match_clauses = Vec::new();
  for i in 0..impl_data.num_variants() {
    let (args, pattern) = impl_data.match_vars(i, "self_");
    let exprs = args.iter().map(|arg| quote!(#arg)).collect::<Vec<_>>();
    let debug_expr = impl_data.debug_expr(i, quote!(f), &exprs);
    match_clauses.push(quote!(#pattern => #debug_expr,));
  }
  impl_data.impl_item(
    quote!(::std::fmt::Debug),
    quote! {
      fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match self {
          #(#match_clauses)*
        }
      }
    },
  )
}
