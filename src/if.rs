use super::*;
use crate::{
    common::{Condition, append_if_statement},
    meta_expr::MetaBlock,
};
use syn::{Block, Ident, Token, Type, spanned::Spanned as _};

pub struct If {
    pub if_token: Token![if],
    pub t: Type,
    pub is_token: IsToken,
    pub condition: Condition,
    pub block: MetaBlock,
    pub else_ifs: Vec<ElseIf>,
    pub else_stmnt: Option<Else>,
}
impl Parse for If {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let if_token = input.parse()?;
        let t = input.parse()?;
        let is_token = input.parse()?;
        let condition = input.parse()?;
        let block = input.parse::<MetaBlock>()?;
        check_metavar_name(block.expr.metavar_name(), &t)?;

        let mut else_ifs = Vec::new();
        let mut else_stmnt = None;

        while !input.is_empty() {
            if input.peek(Token![else]) {
                if input.peek2(Token![if]) {
                    // Encountered else-if
                    else_ifs.push(input.parse()?);
                } else {
                    // Encountered else
                    let new = input.parse::<Else>()?;
                    match else_stmnt {
                        Some(_) => return Err(syn::Error::new(new.else_token.span, "Two 'else' statements are not allowed")),
                        None => else_stmnt = Some(new),
                    }
                }
            } else {
                return Err(input.error("Invalid tokens: 'else' should be the last statement"));
            }
        }

        Ok(Self {
            if_token,
            t,
            is_token,
            condition,
            block,
            else_ifs,
            else_stmnt,
        })
    }
}
impl ToTokens for If {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        append_if_statement(
            self.if_token.span(),
            None,
            &self.t,
            self.is_token.0.span(),
            &self.condition,
            &self.block.braces,
            &self.block.expr,
            tokens,
        );

        for else_if in &self.else_ifs {
            append_if_statement(
                else_if.if_token.span(),
                Some(else_if.else_token.span()),
                &else_if.t,
                else_if.is_token.0.span(),
                &else_if.condition,
                &else_if.block.braces,
                &else_if.block.expr,
                tokens,
            );
        }

        if let Some(else_stmnt) = &self.else_stmnt {
            else_stmnt.else_token.to_tokens(tokens);
            else_stmnt.block.to_tokens(tokens);
        }
    }
}
impl Display for If {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "if {t} is {cond} {{\n\t...\n}} ", t = self.t.to_token_stream(), cond = self.condition)?;
        for else_if in &self.else_ifs {
            write!(f, "else if {t} is {cond} {{\n\t...\n}} ", t = self.t.to_token_stream(), cond = else_if.condition)?;
        }
        if self.else_stmnt.is_some() {
            write!(f, "else {{\n\t...\n}}")?;
        }
        Ok(())
    }
}
impl Debug for If {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("If")
            .field("t", &self.t.to_token_stream().to_string())
            .field("condition", &self.condition.to_string())
            .field("else_ifs", &self.else_ifs)
            .field("else", &match &self.else_stmnt {
                Some(_) => "Some",
                None => "None",
            })
            .finish()
    }
}

pub struct ElseIf {
    pub else_token: Token![else],
    pub if_token: Token![if],
    pub t: Type,
    pub is_token: IsToken,
    pub condition: Condition,
    pub block: MetaBlock,
}
impl Parse for ElseIf {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let rtrn = Self {
            else_token: input.parse()?,
            if_token: input.parse()?,
            t: input.parse()?,
            is_token: input.parse()?,
            condition: input.parse()?,
            block: input.parse()?,
        };
        check_metavar_name(rtrn.block.expr.metavar_name(), &rtrn.t)?;
        Ok(rtrn)
    }
}
impl Debug for ElseIf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ElseIf")
            .field("t", &self.t.to_token_stream().to_string())
            .field("condition", &self.condition.to_string())
            .finish()
    }
}

pub struct Else {
    pub else_token: Token![else],
    pub block: Block,
    // Don't embed else-ifs here (make different type ElseIf) to avoid recursion.
}
impl Parse for Else {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            else_token: input.parse()?,
            block: input.parse()?,
        })
    }
}

#[derive(Clone)]
pub struct IsToken(pub Ident);
impl Parse for IsToken {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.parse::<Ident>()?;
        if ident == "is" {
            Ok(Self(ident))
        } else {
            Err(syn::Error::new(ident.span(), "Expected 'is' meta-keyword"))
        }
    }
}

fn check_metavar_name(metavar: Option<&str>, t: &Type) -> syn::Result<()> {
    // The metavariables in the body must match the Type named by the user
    if let Some(name) = metavar {
        let t_str = t.to_token_stream().to_string();
        if name != t_str {
            return Err(syn::Error::new(t.span(), format!("The metavariables (${name}) must match the generic type provided ({t_str}).")));
        }
    }
    Ok(())
}
