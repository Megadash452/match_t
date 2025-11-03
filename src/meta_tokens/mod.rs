mod parse;
mod stream;
mod token;

use std::{fmt::{Debug, Display, Write as _}, rc::Rc};
use proc_macro2::{Group, Delimiter, Ident, Punct, Spacing, Span, TokenStream, TokenTree};
use syn::{Type, braced, parse::{Parse, ParseStream}, token::Brace};
use stream::MetaTokenStream;
use token::{MetaToken, MetaCastType};

/// Akin to an [`Expr`], a [`MetaExpr`] is an expression of rust code that contains a *type* **metavariable** that must be resolved to a *concrete type*.
///
/// A [`MetaExpr`] is a modified version of a [`TokenStream`] with the ability to hold custom tokens.
/// The custom token is a [`MetaVariable`], which is a placeholder for a **concrete type**.
/// When a [`MetaExpr`] is converted [to `TokenStream`][MetaExpr::to_tokens()],
/// all [`MetaVariable`] placeholders are converted to the **concrete type**.
/// 
/// A [`MetaExpr`] is a series of [`TokenStream`] with [`MetaVariable`] placeholders in between.
/// When the **metavariables** are resolved to a *concrete type*,
/// the tokens are concatenated together to form a single [`TokenStream`].
///
/// [`MetaExpr`]s can only use *one single* **metavariable** name.
/// This means that if the [`MetaExpr`] has tokens `$T`, it can't also have `$G`.
/// It can have multiple instances of the **metavariable**, but all must have the same name.
///
/// The [`MetaExprInner`] is wrapped in an [`Rc`] because *deep clones* of the `TokenTree` are unnecessary work,
/// and since [`MetaExprInner`] is *immutable*, it's best to have all clones share the same `TokenTree`.
/// 
/// [`MetaVariable`]: MetaToken::MetaVar
pub struct MetaExpr(Rc<MetaExprInner>);
impl MetaExpr {
    pub fn metavar_name(&self) -> Option<&str> {
        self.0.metavar_name.as_deref()
    }
    
    /// Like [`Parse::parse()`], but the caller provides a **metavariable name**
    /// so that [`MetaVariable`]s are checked when they are parsed.
    /// This check is necessary because all [`MetaVariable`]s in the same [`MetaExpr`] must have the same **name**.
    /// 
    /// This function takes *all* the tokens in the [`ParseStream`].
    /// 
    /// [`MetaVariable`]: MetaToken::MetaVar
    pub fn parse(input: ParseStream, metavar_name: &str) -> syn::Result<Self> {
        Self::parse_tokens(TokenStream::parse(input)?, metavar_name)
    }
    /// Same as [`Self::parse()`], but takes a [`TokenStream`] instead.
    pub fn parse_tokens(tokens: TokenStream, metavar_name: &str) -> syn::Result<Self> {
        parse::parse_as_metatokens(tokens, metavar_name)
            .map(|tokens| Self(Rc::new(MetaExprInner {
                tokens,
                metavar_name: Some(metavar_name.to_string()),
            })))
    }

    /// Like [`quote::ToTokens::to_tokens()`], but takes a [`Type`] to resovle [metavariables][MetaToken::MetaVar] to.
    pub fn to_tokens(&self, ty: &Type, tokens: &mut TokenStream) {
        self.0.tokens.to_tokens(ty, tokens)
    }
}
impl Clone for MetaExpr {
    fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
    }
}
impl Display for MetaExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <MetaTokenStream as Display>::fmt(&self.0.tokens, f)
    }
}
impl Debug for MetaExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <MetaExprInner as Debug>::fmt(&self.0, f)
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

#[derive(Debug)]
struct MetaExprInner {
    tokens: MetaTokenStream,
    /// The *name* of the **metavariable** used in the *tokens* (if any).
    ///
    /// For example, if the *tokens* contains `$T`, then the *name* is `"T"`.
    metavar_name: Option<String>,
}
impl Display for MetaExprInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <MetaTokenStream as Display>::fmt(&self.tokens, f)
    }
}
