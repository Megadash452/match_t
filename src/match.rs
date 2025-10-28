use super::*;
use crate::{common::Condition, meta_expr::{MetaBlock, MetaExpr}};
use quote::{TokenStreamExt as _, quote, quote_spanned};
use syn::{Token, Type, braced, spanned::Spanned as _, token::Brace};

pub struct Match {
    match_token: Token![match],
    t: Type,
    braces: Brace,
    arms: Vec<MatchArm>,
    default_case_arm: Option<(Token![=>], MetaExpr)>,
}
impl Parse for Match {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let match_token = input.parse()?;
        let t = input.parse()?;
        let match_body;
        let braces = braced!(match_body in input);
        let mut arms = Vec::<MatchArm>::new();
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
                    return Err(match_body
                        .error("Unexpected tokens: Default case must be the last case"));
                }

                break;
            }
            // Parse normal case
            arms.push(match_body.parse()?);
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
        let if_token = quote_spanned!(self.match_token.span()=> if);
        let else_token = quote_spanned!(self.match_token.span()=> else);

        for (i, arm) in self.arms.iter().enumerate() {
            // First arm's condition gets `if`, all other arms get `if-else`
            if i == 0 {
                if_token.to_tokens(tokens);
            } else {
                tokens.append_all(quote!(#if_token #else_token))
            }

            arm.case.to_tokens(&self.t, arm.arrow_token.span(), tokens);
            arm.body.to_tokens(&self.braces, tokens);
        }

        if let Some(default_case_arm) = &self.default_case_arm {
            else_token.to_tokens(tokens);
            // If statement block must have braces, so add them if they are not in the arm's body.
            self.braces.surround(tokens, |tokens| default_case_arm.1.to_tokens(tokens));
        }
    }
}
impl Display for Match {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "match {t} {{", t = self.t.to_token_stream())?;
        for arm in &self.arms {
            writeln!(f, "\t{case} => {{ {body} }},", case = arm.case, body = match &arm.body {
                MatchBody::Block { block, .. } => block.to_string(),
                MatchBody::Expr { expr, .. } => expr.to_token_stream().to_string(),
            })?;
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
    body: MatchBody,
}
impl Parse for MatchArm {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            case: input.parse()?,
            arrow_token: input.parse()?,
            body: input.parse()?,
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

enum MatchBody {
    Block {
        block: MetaBlock,
        /// Match arm with braces can **skip** comma.
        _comma: Option<Token![,]>,
    },
    Expr {
        expr: MetaExpr,
        /// Match arm with braces **requires** a comma.
        _comma: Token![,],
    },
}
impl MatchBody {
    /// Same as [`ToTokens::to_tokens()`], but takes in a **default_braces** for [`MatchBody::Expr`], which will have no **braces**.
    fn to_tokens(&self, default_braces: &Brace, tokens: &mut TokenStream) {
        match self {
            Self::Block { block, .. } => block.to_tokens(tokens),
            Self::Expr { expr, .. } => {
                // If statement block must have braces, so add them if they are not in the arm's body.
                default_braces.surround(tokens, |tokens| expr.to_tokens(tokens))
            }
        }
    }
}
impl Parse for MatchBody {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Brace) {
            Ok(Self::Block {
                block: input.parse()?,
                _comma: input.parse()?,
            })
        } else {
            Ok(Self::Expr {
                expr: input.parse()?,
                _comma: input.parse()?,
            })
        }
    }
}
