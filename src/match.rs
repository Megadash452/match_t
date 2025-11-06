use super::*;
use crate::{
    common::{Condition, TailCast},
    r#if::{Else, ElseIf, IsToken},
    meta_tokens::{MetaBlock, MetaExpr},
};
use proc_macro2::{Span, TokenTree};
use quote::{TokenStreamExt as _, quote_spanned};
use syn::{Expr, Token, Type, braced, spanned::Spanned as _, token::Brace};

pub struct Match {
    match_token: Token![match],
    t: Type,
    braces: Brace,
    arms: Vec<MatchArm>,
    default_case_arm: Option<DefaultArm>,
    tail_cast: Option<TailCast>,
}
impl Match {
    /// Directly converts the [`Match`] statement to an equivalent [`If`] statement.
    pub fn to_if(&self) -> syn::Result<If> {
        // Exit if there are no conditions/blocks to output
        // Also can't convert it if it only one default arm
        if self.arms.is_empty() /* && self.default_case_arm.is_none() */ {
            return Err(syn::Error::new(Span::call_site(), "Can't convert a `match` statement with no conditions to an `if` statement."));
        }

        // At this point, arms is guaranteed to contain elements
        let first_arm = self.arms.first().unwrap();

        let if_token = syn::parse2::<Token![if]>(quote_spanned!(self.match_token.span()=> if)).unwrap();
        let else_token = syn::parse2::<Token![else]>(quote_spanned!(self.match_token.span()=> else)).unwrap();
        let mut is_token = syn::parse2::<IsToken>(quote_spanned!(first_arm.arrow_token.span()=> is)).unwrap();

        Ok(If {
            if_token,
            t: self.t.clone(),
            is_token: is_token.clone(),
            condition: first_arm.case.clone(),
            block: MetaBlock {
                braces: match first_arm.braces {
                    Some(braces) => braces,
                    None => self.braces,
                },
                expr: first_arm.body.clone(),
            },
            else_ifs: self.arms[1..]
                .iter()
                .map(|arm| ElseIf {
                    else_token,
                    if_token,
                    t: self.t.clone(),
                    is_token: {
                        is_token.0.set_span(arm.arrow_token.span());
                        is_token.clone()
                    },
                    condition: arm.case.clone(),
                    block: MetaBlock {
                        braces: match arm.braces {
                            Some(braces) => braces,
                            None => self.braces,
                        },
                        expr: arm.body.clone(),
                    },
                })
                .collect(),
            else_stmnt: self.default_case_arm
                .as_ref()
                .map(|default_arm| Else {
                    else_token,
                    block: match &default_arm.expr {
                        // Expr (with braces) is already a block, don't need to wrap it in another block
                        Expr::Block(block)
                        if block.label.is_none()
                        && block.attrs.is_empty() => block.block.clone(),
                        // Wrap Expr (no braces) in a block
                        _ => syn::Block {
                            brace_token: self.braces,
                            stmts: vec![syn::Stmt::Expr(default_arm.expr.clone(), None)],
                        },
                    },
                }),
            tail_cast: self.tail_cast.clone(),
        })
    }
}
impl Parse for Match {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let match_token = input.parse()?;
        let t = input.parse::<Type>()?;
        let metavar_name = t.to_token_stream().to_string();
        let match_body;
        let braces = braced!(match_body in input);
        let mut arms = Vec::new();
        let mut default_case_arm = None;

        while !match_body.is_empty() {
            // When the case arm is just `_`, that goes in the default_case_arm.
            if let Ok(wild) = match_body.parse::<Token![_]>() {
                default_case_arm = Some(DefaultArm {
                    _wild: wild,
                    _arrow: match_body.parse()?,
                    expr: match_body.parse()?,
                    // Comma is optional at the last arm
                    _comma: match_body.parse()?,
                });

                // default_case must also be the LAST case
                if !match_body.is_empty() {
                    return Err(match_body.error("Unexpected tokens: Default case must be the last case."));
                }

                break;
            }

            // Parse normal case
            arms.push(MatchArm::parse_with_name(&match_body, &metavar_name)?);
        }

        let tail_cast = TailCast::parse_optional_with_name(input, &metavar_name)?;

        if !input.is_empty() {
            return Err(syn::Error::new(input.span(), "Unexpected tokens: Match statement can't have any more tokens"));
        }

        Ok(Self {
            match_token,
            t,
            braces,
            arms,
            default_case_arm,
            tail_cast,
        })
    }
}
impl ToTokens for Match {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        // Exit if there are no conditions/blocks to output
        if self.arms.is_empty() && self.default_case_arm.is_none() {
            return;
        }

        // Match only has default arm, just output the body directly
        if self.arms.is_empty()
        && let Some(default_arm) = &self.default_case_arm {
            default_arm.expr.to_tokens(tokens);
            return;
        }

        // Can unwrap because already handled error cases
        self.to_if().unwrap().to_tokens(tokens);
    }
}
impl Display for Match {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "match {t} {{", t = self.t.to_token_stream())?;
        for arm in &self.arms {
            writeln!(f, "\t{case} => {{ {body} }},", case = arm.case, body = arm.body)?;
        }
        if let Some(default_arm) = &self.default_case_arm {
            writeln!(f, "\t_ => {expr}", expr = default_arm.expr.to_token_stream())?;
        }
        write!(f, "\n}}")
    }
}
impl Debug for Match {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Match")
            .field("t", &self.t.to_token_stream().to_string())
            .field("arms", &self.arms)
            .field("default_case_arm", &match self.default_case_arm {
                Some(_) => "Some",
                None => "None"
            })
            .finish()
    }
}

struct MatchArm {
    case: Condition,
    arrow_token: Token![=>],
    braces: Option<Brace>,
    body: MetaExpr,
    _comma: Option<Token![,]>,
}
impl MatchArm {
    /// Like [`Parse::parse()`], but requires a **metavariable name**.
    fn parse_with_name(input: ParseStream, metavar_name: &str) -> syn::Result<Self> {
        let case = input.parse()?;
        let arrow_token = input.parse()?;

        let mut braces = None;
        let comma;
        let body;

        if input.peek(Brace) {
            let inner_body;
            braces = Some(braced!(inner_body in input));
            body = MetaExpr::parse(&inner_body, metavar_name)?;
            // Arm with braces can have comma, but not required
            comma = input.parse()?;
        } else {
            // Collect tokens to parse as MetaExpr while parsing for comma.
            let mut tokens = TokenStream::new();

            // Parse until comma first so that MetaExpr doesn't eat all the tokens
            comma = input.step(|cursor| {
                let mut cursor = *cursor; // Why they made cursor StepCursor and not Cursor??

                while let Some((tt, next)) = cursor.token_tree() {
                    cursor = next;

                    // FIXME: skip angle bracket groups

                    if let TokenTree::Punct(punct) = &tt
                    && punct.as_char() == ',' {
                        // Convert punct to Token
                        let mut tokens = TokenStream::new();
                        tokens.append(TokenTree::Punct(punct.clone()));
                        let token = syn::parse2::<Token![,]>(tokens).unwrap();
                        return Ok((Some(token), cursor));
                    }

                    // Token was not the comma, append it to the tokens to parse as MetaExpr
                    tokens.append(tt);
                }

                // Did not find comma, but that's ok because last arm can ommit comma.
                Ok((None, cursor))
            })?;

            // Parse MetaExpr from collected tokens
            body = MetaExpr::parse_tokens(tokens, metavar_name)?;
        }

        Ok(Self {
            case,
            arrow_token,
            body,
            braces,
            _comma: comma,
        })
    }
}
impl Debug for MatchArm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MatchArm")
            .field("case", &self.case.to_string())
            .finish()
    }
}

struct DefaultArm {
    _wild: Token![_],
    _arrow: Token![=>],
    expr: Expr,
    _comma: Option<Token![,]>,
}
