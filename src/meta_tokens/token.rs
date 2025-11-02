use super::*;
use proc_macro2::{Literal, extra::DelimSpan};
use quote::ToTokens;
use syn::Token;

/// A [`TokenTree`] that can hold a [`Metavar`].
pub enum MetaToken {
    Group {
        delim: Delimiter,
        span: DelimSpan,
        tokens: MetaTokenStream,
    },
    MetaVar {
        #[allow(unused)]
        dollar: Token![$],
        /// Name of the generic Type (e.g. `T`).
        t: Ident,
    },
    /// There are 2 types of `MetaCast`:
    ///  * A cast that contains a [`MetaVar`] (`$T`).
    ///    This cast converts a *value* with a **generic type** `T` to the **concrete type**.
    ///  * A cast that contains a **generic type** (`T`).
    ///    This cast converts a *value* with a **concrete type** to the **generic type**.
    /// 
    /// While both *types of cast* are mutually exclusive (a Type with *metavariables* can't contain *generics* in the input),
    /// the `T` in both are represented by a [`MetaVar`][MetaToken::MetaVar].
    /// This is done so that there is a placeholder to replace with any type that is needed in the cast.
    MetaCast {
        expr: MetaTokenStream,
        #[allow(unused)]
        as_token: Token![as],
        ty: MetaTokenStream,
        /// Name of the generic Type (e.g. `T`).
        t: String,
        cast_ty: MetaCastType,
    },
    Ident(Ident),
    Punct(Punct),
    Lit(Literal),
}
impl MetaToken {
    /// See [`MetaExpr::parse_all()`].
    /// WARNING: Only use when you know that the token is not 'as' or '$', as it will mess up the MetaTokenStream parsing.
    /// This function is perfectly safe otherwise.
    pub unsafe fn from_tt(tt: TokenTree, parsing_type: bool) -> syn::Result<Self> {
        Ok(match tt {
            TokenTree::Group(group) => Self::Group {
                delim: group.delimiter(),
                span: group.delim_span(),
                tokens: utils::parse_all(group.stream(), parsing_type)?.tokens,
            },
            TokenTree::Ident(ident) => Self::Ident(ident),
            TokenTree::Punct(punct) => Self::Punct(punct),
            TokenTree::Literal(lit) => Self::Lit(lit),
        })
    }
}
/// Equality means that the tokens have the same data, but not necessarily the same [`Span`].
impl PartialEq for MetaToken {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Group {
                delim: self_delim,
                tokens: self_tokens,
                ..
            }, Self::Group {
                delim: other_delim,
                tokens: other_tokens,
                ..
            }) => self_delim == other_delim && self_tokens == other_tokens,
            (Self::Ident(self_ident), Self::Ident(other_ident)) => self_ident.to_string() == other_ident.to_string(),
            (Self::Punct(self_punct), Self::Punct(other_punct)) => self_punct.as_char() == other_punct.as_char() && self_punct.spacing() == other_punct.spacing(),
            (Self::Lit(self_lit), Self::Lit(other_lit)) => self_lit.to_token_stream().to_string() == other_lit.to_token_stream().to_string(),
            _ => false,
        }
    }
}
impl Display for MetaToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MetaToken::Group { delim, tokens, .. } => {
                let (open, close) = match delim {
                    Delimiter::Brace => ("{", "}"),
                    Delimiter::Bracket => ("[", "]"),
                    Delimiter::Parenthesis => ("(", ")"),
                    Delimiter::None => ("", ""),
                };
                f.write_str(open)?;
                <MetaTokenStream as Display>::fmt(tokens, f)?;
                f.write_str(close)
            },
            MetaToken::MetaVar { t, .. } => write!(f, "${t}"),
            MetaToken::MetaCast { expr, ty, .. } => write!(f, "{expr} as {ty}"),
            MetaToken::Ident(ident) => f.write_str(&ident.to_string()),
            MetaToken::Lit(lit) => f.write_str(&lit.to_string()),
            MetaToken::Punct(punct) => f.write_char(punct.as_char()),
        }
    }
}
impl Debug for MetaToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MetaToken::Group {
                delim,
                tokens,
                ..
            } => f.debug_struct("Group")
                .field("delim", &delim)
                .field("tokens", tokens)
                .finish(),
            MetaToken::MetaVar { t, .. } => write!(f, "MetaVar(${t})"),
            MetaToken::MetaCast { expr, ty, cast_ty, .. } => f.debug_struct("MetaCast")
                .field("expr", expr)
                .field("ty", ty)
                .field("cast_ty", cast_ty)
                .finish(),
            MetaToken::Ident(ident) => write!(f, "Ident({:?})", ident.to_string()),
            MetaToken::Punct(punct) => write!(f, "Punct({:?})", punct.as_char()),
            MetaToken::Lit(lit) => write!(f, "Lit({lit})"),
        }
    }
}

#[derive(Debug)]
pub enum MetaCastType {
    /// `as $T`.
    GenericToConcrete,
    /// `as T`.
    ConcreteToGeneric,
}
