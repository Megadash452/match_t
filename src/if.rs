use super::*;
use crate::{common::Condition, meta_tokens::MetaBlock};
use either::Either;
use quote::quote_spanned;
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
        append_if_statement(Either::Left(self), tokens);

        for else_if in &self.else_ifs {
            append_if_statement(Either::Right(else_if), tokens);
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

/// Appends Rust tokens of a single [`If`] or [`ElseIf`] statement.
/// 
/// That is, this function outputs the `else if`, condition, and block.
fn append_if_statement(if_or_elseif: Either<&If, &ElseIf>, tokens: &mut TokenStream) {
    let (if_token, else_token, generic_t, is_token, condition, block);
    match if_or_elseif {
        Either::Right(els) => {
            if_token = els.if_token;
            else_token = els.else_token;
            generic_t = &els.t;
            is_token = &els.is_token;
            condition = &els.condition;
            block = &els.block;
        },
        Either::Left(if_) => {
            if_token = if_.if_token;
            else_token = syn::parse2(quote_spanned!(if_.if_token.span()=> else)).unwrap();
            generic_t = &if_.t;
            is_token = &if_.is_token;
            condition = &if_.condition;
            block = &if_.block;
        },
    };

    // Can't output a first `if` statement if caller is requesting all `else-if` statements.
    let mut is_first = if_or_elseif.is_left();
    // Why closure behaving weird and capturing tokens forever? :/
    fn append_if_tokens(
        is_first: &mut bool,
        if_token: Token![if],
        else_token: Token![else],
        tokens: &mut TokenStream,
    ) {
        // First arm's condition gets `if`, all other arms get `else-if`
        if *is_first {
            if_token.to_tokens(tokens);
            *is_first = false;
        } else {
            else_token.to_tokens(tokens);
            if_token.to_tokens(tokens);
        }
    }

    if block.expr.metavar_name().is_some() {
        // When an if's block has a metavariable, each condition must be put in a different `else-if` block.
        // The metavar_name check is done in Parse
        for cond_ty in condition {
            append_if_tokens(&mut is_first, if_token, else_token, tokens);
            Condition::single_to_tokens(cond_ty, generic_t, is_token.0.span(), tokens);
            // Clone the same body for each block.
            // Metavariables are resolved to cond_ty
            block.braces.surround(tokens, |tokens| {
                block.expr.to_tokens(cond_ty, tokens);
            });
        }
    } else {
        // No metavariables in the body means we output a single normal `if/else-if` statement
        append_if_tokens(&mut is_first, if_token, else_token, tokens);
        condition.to_tokens(generic_t, is_token.0.span(), tokens);
        block.braces.surround(tokens, |tokens| {
            // Provide a fake Type since there are no metavariables to resolve
            block.expr.to_tokens(&Type::Verbatim(TokenStream::new()), tokens);
        });
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
