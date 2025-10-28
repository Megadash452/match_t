use super::*;
use crate::{
    common::{Condition, append_if_statement},
    meta_expr::MetaExpr,
};
use quote::TokenStreamExt as _;
use syn::{Expr, Token, Type, braced, spanned::Spanned as _, token::Brace};

pub struct Match {
    match_token: Token![match],
    t: Type,
    braces: Brace,
    arms: Vec<MatchArm>,
    default_case_arm: Option<(Token![=>], Expr)>,
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
        for arm in self.arms.iter() {
            let braces = match &arm.braces {
                Some(braces) => braces,
                None => &self.braces,
            };
            append_if_statement(
                self.match_token.span(),
                None,
                &self.t,
                arm.arrow_token.span(),
                &arm.case,
                braces,
                &arm.body,
                tokens,
            );
        }

        if let Some(default_case_arm) = &self.default_case_arm {
            tokens.append(proc_macro2::Ident::new("else", self.match_token.span()));
            // If statement block must have braces, so add them if they are not in the arm's body.
            self.braces.surround(tokens, |tokens| {
                default_case_arm.1.to_tokens(tokens)
            });
        }
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
            case: input.parse()?,
            arrow_token: input.parse()?,
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
