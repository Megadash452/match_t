use super::*;
use proc_macro2::{Delimiter, Group, Literal, Punct, Span, TokenTree, extra::DelimSpan};
use quote::TokenStreamExt as _;
use std::{fmt::Write, rc::Rc};
use syn::{Token, Type, braced, token::Brace};

/// Akin to an [`Expr`], a [`MetaExpr`] is an expression of rust code that contains a *type* **metavariable** that must be resolved to a *concrete type*.
///
/// A [`MetaExpr`] is a series of [`TokenStream`] with **metavariable** placeholders in between.
/// When the **metavariables** are resolved to a *concrete type*,
/// the tokens are concatenated together to form a single [`TokenStream`].
///
/// [`MetaExpr`]s can only use *one single* **metavariable** name.
/// This means that if the [`MetaExpr`] has tokens `$T`, it can't also have `$G`.
/// It can have multiple instances of the **metavariable**, but all must have the same name.
///
/// The [`MetaExprInner`] is wrapped in an [`Rc`] because *deep clones* of the `TokenTree` are unnecessary work,
/// and since [`MetaExprInner`] is *immutable*, it's best to have all clones share the same `TokenTree`.
pub struct MetaExpr(Rc<MetaExprInner>);
struct MetaExprInner {
    tokens: MetaTokenStream,
    /// The *name* of the **metavariable** used in the *tokens* (if any).
    ///
    /// For example, if the *tokens* contains `$T`, then the *name* is `"T"`.
    metavar_name: Option<String>,
}
impl MetaExpr {
    pub fn metavar_name(&self) -> Option<&str> {
        self.0.metavar_name.as_deref()
    }

    /// Converts all tokens in a [`TokenStream`] into [`MetaToken`].
    ///
    /// Returns error if more than 1 **metavariable** names are found.
    fn parse_all(stream: TokenStream) -> syn::Result<MetaExprInner> {
        let mut tokens = Vec::new();
        let mut metavar_name = None::<String>;

        fn multiple_metavar_names_error(name_1: &str, name_2: &str, span: Span) -> syn::Error {
            syn::Error::new(span, format!("Only one Metavariable name can be used within a branch. Found ${name_2} while ${name_1} exists."))
        }

        let mut token_iter = stream.into_iter();

        while let Some(tt) = token_iter.next() {
            match tt {
                TokenTree::Group(group) => {
                    let inner_expr = Self::parse_all(group.stream())?;

                    // This frame inherits the inner MetaExpr's metavar_name (unless it already has one)
                    match (&mut metavar_name, inner_expr.metavar_name) {
                        (Some(name), Some(inner_name))
                        if name != inner_name.as_str()
                            => return Err(multiple_metavar_names_error(name, &inner_name, Span::call_site())),
                        (None, inner_metavar_name) => metavar_name = inner_metavar_name.clone(),
                        (Some(_), _) => { },
                    }

                    tokens.push(MetaToken::Group {
                        delim: group.delimiter(),
                        span: group.delim_span(),
                        tokens: inner_expr.tokens,
                    })
                },
                TokenTree::Punct(punct) => {
                    // Speculatively advance cursor
                    if punct.as_char() == '$'
                    && let Some(TokenTree::Ident(ident)) = token_iter.clone().next() {
                        // Matched a metavariable! Push it to tokens and advance Cursor.
                        token_iter.next().unwrap();

                        match &metavar_name {
                            Some(name) => if ident != name {
                                return Err(multiple_metavar_names_error(name, &ident.to_string(), ident.span()));
                            },
                            _ => metavar_name = Some(ident.to_string()),
                        }

                        tokens.push(MetaToken::MetaVar {
                            _dollar: syn::parse2(punct.to_token_stream()).unwrap(),
                            t: ident,
                        });
                    } else {
                        // Did not match a metavariable, rerun iteration with this new token
                        // (do not advance cursor)
                        tokens.push(MetaToken::Punct(punct))
                    }
                },
                TokenTree::Ident(ident) => tokens.push(MetaToken::Ident(ident)),
                TokenTree::Literal(lit) => tokens.push(MetaToken::Lit(lit)),
            }
        }

        Ok(MetaExprInner {
            tokens: MetaTokenStream(tokens),
            metavar_name,
        })
    }

    /// Converts the [`MetaExpr] back to a normal Rust expression/block of code.
    ///
    /// The **metavariables** (if any) are resolved to the **type** that is passed in.
    pub fn to_token_stream(&self, ty: &Type) -> TokenStream {
        Self::to_token_stream_impl(&self.0.tokens.0, ty)
    }
    fn to_token_stream_impl(tokens: &[MetaToken], ty: &Type) -> TokenStream {
        let mut stream = TokenStream::new();

        for meta_token in tokens {
            match meta_token {
                MetaToken::Group {
                    delim,
                    span,
                    tokens,
                } => {
                    let inner_stream = Self::to_token_stream_impl(&tokens.0, ty);
                    let mut group = Group::new(*delim, inner_stream);
                    group.set_span(span.join());
                    stream.append(group);
                },
                MetaToken::MetaVar { .. } => stream.append_all(clear_span(ty.to_token_stream())),
                MetaToken::Ident(ident) => ident.to_tokens(&mut stream),
                MetaToken::Lit(lit) => lit.to_tokens(&mut stream),
                MetaToken::Punct(punct) => punct.to_tokens(&mut stream),
            }
        }

        stream
    }
}
impl Parse for MetaExpr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Self::parse_all(TokenStream::parse(input)?)
            .map(|inner| Self(Rc::new(inner)))
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
        <MetaTokenStream as Debug>::fmt(&self.0.tokens, f)
    }
}

/// Same as a [`MetaExpr`], but requires tokens to be wrapped in [`Braces`][Brace].
#[derive(Clone)]
pub struct MetaBlock {
    pub braces: Brace,
    pub expr: MetaExpr,
}
impl Parse for MetaBlock {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let inner;
        let braces = braced!(inner in input);

        Ok(Self {
            braces,
            expr: inner.parse()?,
        })
    }
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

#[derive(Clone)]
struct MetaTokenStream(Vec<MetaToken>);
impl Display for MetaTokenStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for meta_token in &self.0 {
            <MetaToken as Display>::fmt(meta_token, f)?;
        }
        Ok(())
    }
}
impl Debug for MetaTokenStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.is_empty() {
            f.write_str("<empty>")
        } else {
            f.debug_list()
                .entries(&self.0)
                .finish()
        }
    }
}

/// A [`TokenTree`] that can hold a [`Metavar`].
#[derive(Clone)]
enum MetaToken {
    Group {
        delim: Delimiter,
        span: DelimSpan,
        tokens: MetaTokenStream,
    },
    MetaVar {
        _dollar: Token![$],
        t: syn::Ident,
    },
    Ident(proc_macro2::Ident),
    Punct(Punct),
    Lit(Literal),
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
            MetaToken::Ident(ident) => write!(f, "Ident({:?})", ident.to_string()),
            MetaToken::Punct(punct) => write!(f, "Punct({:?})", punct.as_char()),
            MetaToken::Lit(lit) => write!(f, "Lit({lit})"),
        }
    }
}

fn clear_span(stream: TokenStream) -> TokenStream {
    let mut stream_buf = TokenStream::new();

    for tt in stream {
        match tt {
            TokenTree::Group(group) => {
                let new_inner = clear_span(group.stream());
                stream_buf.append(Group::new(group.delimiter(), new_inner));
            },
            mut tt => {
                tt.set_span(Span::call_site());
                stream_buf.append(tt);
            },
        }
    }

    stream_buf
}
