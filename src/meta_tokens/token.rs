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
    MetaVar(MetaVar),
    MetaCast(MetaCast),
    Ident(Ident),
    Punct(Punct),
    Lit(Literal),
}
impl MetaToken {
    /// Converts a [`TokenTree::Group`] to a [`MetaToken::Group`] by parsing the [`Group`]'s inner [`TokenStream`] with a given **parser**.
    pub fn from_group(group: Group, parser: impl FnOnce(TokenStream) -> syn::Result<MetaTokenStream>) -> syn::Result<Self> {
        Ok(Self::Group {
            delim: group.delimiter(),
            span: group.delim_span(),
            tokens: parser(group.stream())?,
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
            (Self::MetaVar(self_metavar), Self::MetaVar(other_metavar)) => self_metavar == other_metavar,
            (Self::MetaCast(self_metacast), Self::MetaCast(other_metacast)) => self_metacast == other_metacast,
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
            Self::Group { delim, tokens, .. } => {
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
            Self::MetaVar(metavar) => <MetaVar as Display>::fmt(metavar, f),
            Self::MetaCast(metacast) => <MetaCast as Display>::fmt(metacast, f),
            Self::Ident(ident) => f.write_str(&ident.to_string()),
            Self::Lit(lit) => f.write_str(&lit.to_string()),
            Self::Punct(punct) => f.write_char(punct.as_char()),
        }
    }
}
impl Debug for MetaToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Group {
                delim,
                tokens,
                ..
            } => f.debug_struct("Group")
                .field("delim", &delim)
                .field("tokens", tokens)
                .finish(),
            Self::MetaVar(metavar) => <MetaVar as Debug>::fmt(metavar, f),
            Self::MetaCast(metacast) => <MetaCast as Debug>::fmt(metacast, f),
            Self::Ident(ident) => write!(f, "Ident({:?})", ident.to_string()),
            Self::Punct(punct) => write!(f, "Punct({:?})", punct.as_char()),
            Self::Lit(lit) => write!(f, "Lit({lit})"),
        }
    }
}

/// Represents a *placeholder* for a **concrete type** (`$T` -> `bool`).
pub struct MetaVar {
    #[allow(unused)]
    pub dollar: Token![$],
    /// Name of the generic Type (e.g. `T`).
    pub t: Ident,
}
/// Equality means that the tokens have the same data, but not necessarily the same [`Span`].
impl PartialEq for MetaVar {
    fn eq(&self, other: &Self) -> bool {
        self.t == other.t
    }
}
impl Display for MetaVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "${}", self.t)
    }
}
impl Debug for MetaVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "MetaVar(${})", self.t)
    }
}

/// There are 2 types of [`MetaCast`]:
///  * A cast that contains a [`MetaVar`] (`$T`).
///    This cast converts a *value* with a **generic type** `T` to the **concrete type**.
///  * A cast that contains a **generic type** (`T`).
///    This cast converts a *value* with a **concrete type** to the **generic type**.
/// 
/// While both *types of cast* are mutually exclusive (a Type with *metavariables* can't contain *generics* in the input),
/// the `T` in both are represented by a [`MetaVar`].
/// This is done so that there is a placeholder to replace with any type that is needed in the cast.
pub struct MetaCast {
    pub expr: MetaTokenStream,
    #[allow(unused)]
    pub as_token: Token![as],
    pub ty: MetaTokenStream,
    /// Name of the generic Type (e.g. `T`).
    pub t: String,
    pub cast_ty: MetaCastType,
}
/// Equality means that the tokens have the same data, but not necessarily the same [`Span`].
impl PartialEq for MetaCast {
    fn eq(&self, other: &Self) -> bool {
        self.cast_ty == other.cast_ty
        && self.expr == other.expr
        && self.ty == other.ty
    }
}
impl Display for MetaCast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} as {}", self.expr, self.ty)
    }
}
impl Debug for MetaCast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MetaCast")
            .field("expr", &self.expr)
            .field("ty", &self.ty)
            .field("cast_ty", &self.cast_ty)
            .finish()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum MetaCastType {
    /// `as $T`.
    GenericToConcrete,
    /// `as T`.
    ConcreteToGeneric,
}
