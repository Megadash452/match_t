use super::*;
use crate::{common::Condition, r#if::{Else, ElseIf, IsToken}, meta_expr::{MetaBlock, MetaExpr}};
use proc_macro2::Span;
use quote::quote_spanned;
use syn::{Expr, Token, Type, braced, spanned::Spanned as _, token::Brace};

pub struct Match {
    match_token: Token![match],
    t: Type,
    braces: Brace,
    arms: Vec<MatchArm>,
    default_case_arm: Option<(Token![=>], Expr)>,
}
impl Match {
    /// Directly converts the [`Match`] statement to an equivalent [`If`] statement.
    #[allow(unused)]
    fn to_if(&self) -> syn::Result<If> {
        // Exit if there are no conditions/blocks to output
        if self.arms.is_empty() {
            return Err(syn::Error::new(Span::call_site(), "Can't convert a `match` statement with no arms to an `if` statement."));
        }

        // Match only has default arm
        if self.arms.is_empty() && let Some((_, _)) = &self.default_case_arm {
            return Err(syn::Error::new(Span::call_site(), "Can't convert a `match` statement with only the default arm to an `if` statement."));
        }

        // At this point, arms is guaranteed to contain elements
        let first_arm = self.arms.first().unwrap();

        let if_token = syn::parse2::<Token![if]>(quote_spanned!(self.match_token.span()=> if)).unwrap();
        let else_token = syn::parse2::<Token![else]>(quote_spanned!(self.match_token.span()=> else)).unwrap();
        let mut is_token = syn::parse2::<IsToken>(quote_spanned!(first_arm.arrow_token.span()=> is)).unwrap();

        Ok(If {
            if_token: if_token.clone(),
            t: self.t.clone(),
            is_token: syn::parse2(quote_spanned!(first_arm.arrow_token.span()=> is)).unwrap(),
            condition: first_arm.case.clone(),
            block: MetaBlock {
                braces: match &first_arm.braces {
                    Some(braces) => braces,
                    None => &self.braces
                }.clone(),
                expr: first_arm.body.clone(),
            },
            else_ifs: self.arms[1..].iter()
                .map(|arm| ElseIf {
                    else_token: else_token.clone(),
                    if_token: if_token.clone(),
                    t: self.t.clone(),
                    is_token: {
                        is_token.0.set_span(arm.arrow_token.span());
                        is_token.clone()
                    },
                    condition: arm.case.clone(),
                    block: MetaBlock {
                        braces: match &arm.braces {
                            Some(braces) => braces,
                            None => &self.braces
                        }.clone(),
                        expr: arm.body.clone(),
                    }
                })
                .collect(),
            else_stmnt: self.default_case_arm
                .as_ref()
                .map(|(_, expr)| Else {
                    else_token: else_token.clone(),
                    block: match expr {
                        // Expr (with braces) is already a block, don't need ot wrap it in another block
                        Expr::Block(block)
                        if block.label.is_none()
                        && block.attrs.is_empty() => block.block.clone(),
                        // Wrap Expr (no braces) in a block
                        _ => syn::Block {
                            brace_token: self.braces.clone(),
                            stmts: vec![syn::Stmt::Expr(expr.clone(), None)],
                        }
                    }
                })
        })
    }
}
impl Parse for Match {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let match_token = input.parse()?;
        let t = input.parse::<Type>()?;
        let match_body;
        let braces = braced!(match_body in input);
        let mut arms = Vec::new();
        let mut default_case_arm = None;

        while !match_body.is_empty() {
            // When the case arm is just `_`, that goes in the default_case_arm.
            if match_body.parse::<Token![_]>().is_ok() {
                default_case_arm = Some((
                    match_body.parse()?,
                    match_body.parse()?,
                ));
                // Comma is optional at the last arm
                match_body.parse::<Option<Token![,]>>()?;

                // default_case must also be the LAST case
                if !match_body.is_empty() {
                    return Err(match_body.error("Unexpected tokens: Default case must be the last case"));
                }

                break;
            }

            // Parse normal case
            let arm = match_body.parse::<MatchArm>()?;
            // The metavariables in the body must match the Type named by the user
            if let Some(name) = arm.body.metavar_name() {
                let t_str = t.to_token_stream().to_string();
                if name != t_str {
                    return Err(syn::Error::new(t.span(), format!("The metavariables (${name}) must match the generic type provided ({t_str}).")));
                }
            }

            arms.push(arm);
        }

        if !input.is_empty() {
            return Err(input.error("Unexpected tokens: 'match' only has 1 body; no other expressions are allowed"));
        }

        Ok(Self {
            match_token,
            t,
            braces,
            arms,
            default_case_arm,
        })
    }
}
impl ToTokens for Match {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        // Exit if there are no conditions/blocks to output
        if self.arms.is_empty() {
            return;
        }

        // Match only has default arm, just output the body directly
        if self.arms.is_empty() && let Some((_, expr)) = &self.default_case_arm {
            expr.to_tokens(tokens);
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
        if let Some((_, expr)) = &self.default_case_arm {
            writeln!(f, "\t_ => {expr}", expr = expr.to_token_stream())?;
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
impl Parse for MatchArm {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let case = input.parse()?;
        let arrow_token = input.parse()?;
        
        let mut braces = None;
        let mut comma = None;
        let body;

        if input.peek(Brace) {
            let inner_body;
            braces = Some(braced!(inner_body in input));
            body = inner_body.parse()?;
            // Arm with braces can have comma, but not rquired
            comma = input.parse()?;
        } else {
            // Parse expr first so that MetaExpr doesn't eat all the tokens
            let expr = input.parse::<Expr>()?;
            body = syn::parse2(expr.into_token_stream())?;
            // Arm without braces requires comma (unless it's last arm)
            if !input.is_empty() {
                comma = Some(input.parse()?);
            }
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
