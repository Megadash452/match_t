use super::*;
use crate::meta_expr::MetaExpr;
use proc_macro2::{Punct, Span};
use quote::{TokenStreamExt as _, quote, quote_spanned};
use syn::{Token, Type, punctuated::Punctuated, spanned::Spanned as _, token::Brace};

pub fn type_of_tokens(ty: &Type) -> TokenStream {
    quote_spanned! {ty.span()=> ::std::any::TypeId::of::<#ty>() }
}

#[allow(clippy::too_many_arguments)]
pub fn append_if_statement(
    if_span: Span,
    else_span: Option<Span>,
    generic_t: &Type,
    eq_span: Span,
    condition: &Condition,
    braces: &Brace,
    meta_expr: &MetaExpr,
    tokens: &mut TokenStream,
) {
    // Can't output a first `if` statement if caller is requesting all `else-if` statements.
    let mut is_first = else_span.is_none();
    // Wy closure behaving weird and capturing tokens forever? :/
    fn append_if_tokens(
        is_first: &mut bool,
        if_span: Span,
        else_span: Option<Span>,
        tokens: &mut TokenStream,
    ) {
        let if_token = quote_spanned!(if_span=> if);
        let else_token = match else_span {
            Some(span) => quote_spanned!(span=> else),
            None => quote_spanned!(if_span=> else),
        };

        // First arm's condition gets `if`, all other arms get `else-if`
        if *is_first {
            if_token.to_tokens(tokens);
            *is_first = false;
        } else {
            else_token.to_tokens(tokens);
            if_token.to_tokens(tokens);
        }
    }

    if meta_expr.metavar_name().is_some() {
        // When an if's block ha a metavariable, each condition must be put in a different `else-if` block.
        // The metavar_name check is done in Parse
        for cond_ty in condition {
            append_if_tokens(&mut is_first, if_span, else_span, tokens);
            Condition::single_to_tokens(cond_ty, generic_t, eq_span, tokens);
            // Clone the same body for each block.
            // Metavariables are resolved to cond_ty
            braces.surround(tokens, |tokens| {
                tokens.append_all(meta_expr.to_token_stream(cond_ty))
            });
        }
    } else {
        // No metavariables in the body means we output a single normal `if/else-if` statement
        append_if_tokens(&mut is_first, if_span, else_span, tokens);
        condition.to_tokens(generic_t, eq_span, tokens);
        // Provide a fake Type since there are no metavariables to resolve
        braces.surround(tokens, |tokens| {
            tokens.append_all(meta_expr.to_token_stream(&Type::Verbatim(TokenStream::new())))
        });
    }
}

#[derive(Clone)]
pub struct Condition(Punctuated<Type, Token![|]>);
impl Condition {
    /// Generate a `boolean` [`Condition`] that compares `T` with all [`Types`][Type] in `Self` for use in an `if` statement.
    ///
    /// **eq_span** is the [`Span`] that will be assigned to the *equal signs* (`==`) in the condition.
    pub fn to_tokens(&self, generic_t: &Type, eq_span: Span, tokens: &mut TokenStream) {
        for pair in self.0.pairs() {
            // Append comparison (`T == ty`)
            Self::single_to_tokens(pair.value(), generic_t, eq_span, tokens);

            if let Some(bitor) = pair.punct() {
                let mut first = Punct::new('|', proc_macro2::Spacing::Joint);
                let mut second = Punct::new('|', proc_macro2::Spacing::Alone);
                first.set_span(bitor.span());
                second.set_span(bitor.span());
                tokens.append(first);
                tokens.append(second);
            }
        }
    }

    /// Same as [`Self::to_tokens()`], but operates on a single [`Type`] instead of all of the types in the [`Condition`].
    pub fn single_to_tokens(ty: &Type, generic_t: &Type, eq_span: Span, tokens: &mut TokenStream) {
        let type_of_ty = type_of_tokens(ty);
        let type_of_t = type_of_tokens(generic_t);
        let eq = {
            let mut first = Punct::new('=', proc_macro2::Spacing::Joint);
            let mut second = Punct::new('=', proc_macro2::Spacing::Alone);
            first.set_span(eq_span);
            second.set_span(eq_span);
            quote!(#first #second)
        };

        tokens.append_all(quote! { #type_of_t #eq #type_of_ty })
    }
}
impl Parse for Condition {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self(Punctuated::parse_separated_nonempty(input)?))
    }
}
impl<'a> IntoIterator for &'a Condition {
    type Item = &'a Type;
    type IntoIter = syn::punctuated::Iter<'a, Type>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}
impl IntoIterator for Condition {
    type Item = Type;
    type IntoIter = syn::punctuated::IntoIter<Type>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}
impl Display for Condition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for pair in self.0.pairs() {
            let ty = pair.value().to_token_stream();
            let sep = match pair.punct() {
                Some(_) => " | ",
                None => "",
            };
            write!(f, "{ty}{sep}")?;
        }
        Ok(())
    }
}
