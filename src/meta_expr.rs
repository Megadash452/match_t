use super::*;
use std::collections::VecDeque;
use proc_macro2::{Delimiter, Literal, Punct, TokenTree, extra::DelimSpan};
use syn::{Expr, Token, braced, buffer::Cursor, token::Brace};

/// Akin to an [`Expr`], a [`MetaExpr`] is an expression of rust code that contains a *type* **metavariable** that must be resolved to a *concrete type*.
/// 
/// A [`MetaExpr`] is a series of [`TokenStream`] with **metavariable** placeholders in between.
/// When the **metavariables** are resolved to a *concrete type*,
/// the tokens are concatenated together to form a single [`TokenStream`].
/// 
/// [`MetaExpr`]s can only use *one single* **metavariable** name.
/// This means that if the [`MetaExpr`] has tokens `$T`, it can't also have `$G`.
/// It can have multiple instances of the **metavariable**, but all must have the same name.
pub struct MetaExpr {
    tokens: Vec<MetaToken>,
    /// The *name* of the **metavariable** used in the *tokens* (if any).
    /// 
    /// For example, if the *tokens* contains `$T`, then the *name* is `"T"`.
    metavar_name: Option<String>,
}
/// A [`TokenTree`] that can hold a [`Metavar`].
enum MetaToken {
    Group {
        delim: Delimiter,
        span: DelimSpan,
        tokens: Vec<Self>,
    },
    MetaVar {
        dollar: Token![$],
        t: syn::Ident,
    },
    Ident(proc_macro2::Ident),
    Punct(Punct),
    Lit(Literal),
}
impl MetaExpr {
    /// Converts all tokens in a [`TokenStream`] into [`MetaToken`].
    /// 
    /// Returns error if more than 1 **metavariable** names are found.
    fn parse_all(cursor: Cursor<'_>) -> syn::Result<Self> {
        // The MetaExpr containing all MetaTokens
        let mut meta_expr = Vec::<MetaToken>::new(); // This is always mutably borrowed during iteration
        let mut metavar_name = None;
        // This is the BFS stack
        let mut cursors = VecDeque::new();
        // It holds a pair of the original TokenStream being parsed, and a reference to the MetaExpr that it will add tokens to
        cursors.push_back((cursor, &mut meta_expr));

        fn next(cursor: &mut Cursor) -> Option<TokenTree> {
            cursor.token_tree()
                .map(|(tt, next)| {
                    *cursor = next;
                    tt
                })
        }

        while let Some((mut cursor, tokens)) = cursors.pop_front() {
            while let Some(tt) = next(&mut cursor) {
                match tt {
                    TokenTree::Group(group) => {
                        // Add the group's inner tokens, along with a new inner MetaTokens to the queue.
                        tokens.push(MetaToken::Group {
                            delim: group.delimiter(),
                            span: group.delim_span(),
                            tokens: Vec::new(),
                        });
                        let tokens = if let MetaToken::Group { tokens, .. } = tokens.last_mut().unwrap() {
                            tokens
                        } else {
                            panic!("UNREACHABLE")
                        };
                        let cursor;
                        syn::parse::Parser::parse2(|input: ParseStream| {
                            cursor = input.cursor();
                            Ok(())
                        }, group.stream());

                        cursors.push_back((cursor, tokens));
                    },
                    TokenTree::Punct(punct) => {
                        // Speculatively advance cursor
                        if punct.as_char() == '$'
                        && let Some(TokenTree::Ident(ident)) = next(&mut cursor.clone()) {
                            // Matched a metavariable! Push it to tokens and advance Cursor.
                            next(&mut cursor).unwrap();

                            match &metavar_name {
                                Some(name) => if ident == name {
                                    return Err(syn::Error::new(ident.span(), "Only one Metavariable name can be used within a branch"));
                                },
                                None => metavar_name = Some(ident.to_string()),
                            }

                            tokens.push(MetaToken::MetaVar {
                                dollar: syn::parse2(punct.to_token_stream()).unwrap(),
                                t: ident.into()
                            });
                        }
                        // Did not match a metavariable, rerun iteration with this new token
                        // (do not advance cursor)
                        tokens.push(MetaToken::Punct(punct))
                    },
                    TokenTree::Ident(ident) => tokens.push(MetaToken::Ident(ident)),
                    TokenTree::Literal(lit) => tokens.push(MetaToken::Lit(lit)),
                }
            }
        }

        // Drop cursors to take back the reference to MetaExpr
        drop(cursors);

        Ok(Self { tokens: meta_expr, metavar_name })
    }
}
impl Parse for MetaExpr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let expr = input.parse::<Expr>()?;
        syn::parse::Parser::parse2(|input: ParseStream| MetaExpr::parse_all(input.cursor()), expr.into_token_stream())
    }
}
impl ToTokens for MetaExpr {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        todo!()
    }
}
impl Display for MetaExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
impl Debug for MetaExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

/// Same as a [`MetaExpr`], but requires tokens to be wrapped in [`Braces`][Brace].
pub struct MetaBlock {
    braces: Brace,
    expr: MetaExpr
}
impl Parse for MetaBlock {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let inner;
        let braces = braced!(inner in input);

        Ok(Self { braces, expr: inner.parse()? })
    }
}
impl ToTokens for MetaBlock {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.braces.surround(tokens, |tokens| self.expr.to_tokens(tokens))
    }
}
impl Display for MetaBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
impl Debug for MetaBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}
