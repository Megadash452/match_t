use super::*;
use crate::meta_tokens::{MetaExpr, stream::MetaTokenStream, token::MetaCastType};
use proc_macro2::{Punct, Span};
use quote::{TokenStreamExt as _, quote, quote_spanned};
use syn::{Token, Type, punctuated::Punctuated, spanned::Spanned as _};

pub fn type_of_tokens(ty: &Type) -> TokenStream {
    quote_spanned! {ty.span()=> ::std::any::TypeId::of::<#ty>() }
}

#[derive(Clone)]
pub struct Condition(Punctuated<Type, Token![|]>);
impl Condition {
    /// Returns an iterator over **references** of [`Type`].
    pub fn iter(&self) -> <&Self as IntoIterator>::IntoIter {
        self.0.iter()
    }

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

/// Like [`MetaCast`][crate::meta_tokens::token::MetaCast], converting an **expression** to some *MetaType*,
/// but only allows converting [from **concrete type** to **generic type**][crate::meta_tokens::token::MetaCastType::ConcreteToGeneric].
///
/// The **expression** is the [`If`] or [`Match`] that contains this [`TailCast`].
#[derive(Clone)]
pub struct TailCast {
    #[allow(unused)]
    pub dollar: Token![$],
    #[allow(unused)]
    pub as_token: Token![as],
    pub ty: MetaExpr,
}
impl TailCast {
    /// Like [`Parse::parse()`], but requires a **metavariable name**.
    pub fn parse_with_name(input: ParseStream, metavar_name: &str) -> syn::Result<Self> {
        let fork = input.fork();
        let dollar = fork.parse::<Token![$]>()?;
        let as_token = fork.parse::<Token![as]>()?;
        drop(fork);
        
        let ty = {
            let token_iter = &mut input.parse::<TokenStream>().unwrap().into_iter();
            let cast = crate::meta_tokens::parse_metacast(
                token_iter,
                &mut MetaTokenStream::new(),
                metavar_name,
            )?.unwrap();

            if cast.cast_ty == MetaCastType::GenericToConcrete {
                return Err(syn::Error::new(as_token.span(), format!("A Generic To Concrete MetaCast is not allowed as a Tail Cast. Don't use metavariable '${metavar_name}'")));
            }
            if let Some(tt) = token_iter.next() {
                return Err(syn::Error::new(tt.span(), "Unexpected tokens: If/Match statement can't have any more tokens"));
            }

            MetaExpr::from(cast.ty)
        };

        Ok(Self { dollar, as_token, ty })
    }
    /// Like [`Self::parse_with_name()`], but can return [`None`].
    pub fn parse_optional_with_name(input: ParseStream, metavar_name: &str) -> syn::Result<Option<Self>> {
        Ok(if input.peek(Token![$]) && input.peek2(Token![as]) {
            Some(Self::parse_with_name(input, metavar_name)?)
        } else {
            None
        })
    }
}
// ToTokens has to be implemented manually by If and Match
