mod parse;
pub mod stream;
pub mod token;

pub use parse::parse_metacast;
use proc_macro2::{Delimiter, Group, Ident, Punct, Spacing, Span, TokenStream, TokenTree};
use std::{
    fmt::{Debug, Display, Write as _},
    ops::Deref,
    rc::Rc,
};
use stream::MetaTokenStream;
use syn::{
    Type, braced,
    parse::{Parse, ParseStream},
    token::Brace,
};
use token::{MetaCast, MetaCastType, MetaToken, MetaVar};

/// Akin to an [`Expr`][syn::Expr], a [`MetaExpr`] is an expression of rust code that contains a *type* **metavariable** that must be resolved to a *concrete type*.
///
/// A [`MetaExpr`] is a modified version of a [`TokenStream`] with the ability to hold custom tokens.
/// The custom token is a [`MetaVar`], which is a placeholder for a **concrete type**.
/// When a [`MetaExpr`] is converted [to `TokenStream`][MetaExpr::to_tokens()],
/// all [`MetaVar`] placeholders are converted to the **concrete type**.
///
/// [`MetaExpr`]s can only use *one single* **metavariable** name.
/// This means that if the [`MetaExpr`] has tokens `$T`, it can't also have `$G`.
/// It can have multiple instances of the **metavariable**, but all must have the same name.
///
/// The [`MetaTokenStream`] is wrapped in an [`Rc`] because *deep clones* of the `TokenTree` are unnecessary work,
/// and since [`MetaTokenStream`] is *immutable*, it's best to have all clones share the same `TokenTree`.
pub struct MetaExpr(Rc<MetaTokenStream>);
impl MetaExpr {
    /// Like [`Parse::parse()`], but the caller provides a **metavariable name**
    /// so that [`MetaVar`]s are checked when they are parsed.
    /// This check is necessary because all [`MetaVar`]s in the same [`MetaExpr`] must have the same **name**.
    ///
    /// This function takes *all* the tokens in the [`ParseStream`].
    pub fn parse(input: ParseStream, metavar_name: &str) -> syn::Result<Self> {
        Self::parse_tokens(TokenStream::parse(input)?, metavar_name)
    }
    /// Same as [`Self::parse()`], but takes a [`TokenStream`] instead.
    pub fn parse_tokens(tokens: TokenStream, metavar_name: &str) -> syn::Result<Self> {
        parse::parse_as_metatokens(tokens, metavar_name).map(|tokens| Self(Rc::new(tokens)))
    }
}
impl Clone for MetaExpr {
    fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
    }
}
impl From<MetaTokenStream> for MetaExpr {
    fn from(value: MetaTokenStream) -> Self {
        Self(Rc::new(value))
    }
}
impl Deref for MetaExpr {
    type Target = MetaTokenStream;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl Display for MetaExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <MetaTokenStream as Display>::fmt(&self.0, f)
    }
}
impl Debug for MetaExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <MetaTokenStream as Debug>::fmt(&self.0, f)
    }
}

/// Same as a [`MetaExpr`], but requires tokens to be wrapped in [`Braces`][Brace].
#[derive(Clone)]
pub struct MetaBlock {
    pub braces: Brace,
    pub expr: MetaExpr,
}
impl MetaBlock {
    /// Same as [`MetaExpr::parse()`], but parses within a [`Brace`].
    pub fn parse(input: ParseStream, metavar_name: &str) -> syn::Result<Self> {
        let inner;

        Ok(Self {
            braces: braced!(inner in input),
            expr: MetaExpr::parse(&inner, metavar_name)?,
        })
    }
    // No parse_tokens() because MetaBlock only takes 1 group token.
    // Taking an entire TokenStream would make no sense.
}
impl Display for MetaBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("{ ")?;
        <MetaExpr as Display>::fmt(&self.expr, f)?;
        f.write_str(" }")
    }
}
impl Debug for MetaBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MetaBlock")
            .field("braces", &"Braces { }")
            .field("expr", &self.expr)
            .finish()
    }
}
