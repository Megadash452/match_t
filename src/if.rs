use super::*;
use crate::{common::{Condition, TailCast}, meta_tokens::{MetaBlock, stream::metacast_to_token_stream, token::MetaCastType}};
use either::Either;
use quote::{quote, quote_spanned};
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
        let t = input.parse::<Type>()?;
        let metavar_name = t.to_token_stream().to_string();
        let is_token = input.parse()?;
        let condition = input.parse()?;
        let block = MetaBlock::parse(input, &metavar_name)?;

        let mut else_ifs = Vec::new();
        let mut else_stmnt = None;

        while input.peek(Token![else]) {
            if input.peek2(Token![if]) {
                // Encountered else-if
                else_ifs.push(input.parse()?);
            } else {
                // Encountered else
                let new = Else::parse_with_name(input, &metavar_name)?;
                match else_stmnt {
                    Some(_) => return Err(syn::Error::new(new.else_token.span, "Two 'else' statements are not allowed")),
                    None => else_stmnt = Some(new),
                }
            }
        }

        // TODO: dont allow TailCast if else_ifs have different generic types
        if input.peek(Token![as]) {
            return Err(syn::Error::new(input.span(), "Tail MetaCast (as ...) is only available if the If statement has an 'else' branch"));
        }
        if !input.is_empty() {
            return Err(syn::Error::new(input.span(), "Unexpected tokens: If statement can't have any more tokens"));
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
        let tail_cast = self.else_stmnt.as_ref()
            .and_then(|els| els.tail_cast.as_ref());

        append_if_statement(Either::Left(self), tail_cast, tokens);

        for else_if in &self.else_ifs {
            append_if_statement(Either::Right(else_if), tail_cast, tokens);
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
        let else_token = input.parse()?;
        let if_token = input.parse()?;
        let t = input.parse::<Type>()?;
        let metavar_name = t.to_token_stream().to_string();
        let is_token = input.parse()?;
        let condition = input.parse()?;
        let block = MetaBlock::parse(input, &metavar_name)?;

        Ok(Self { else_token, if_token, t, is_token, condition, block })
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
    pub tail_cast: Option<TailCast>,
}
impl Else {
    /// Like [`Parse::parse()`], but requires a **metavariable name**.
    fn parse_with_name(input: ParseStream, metavar_name: &str) -> syn::Result<Self> {
        Ok(Self {
            else_token: input.parse()?,
            block: input.parse()?,
            tail_cast: TailCast::parse_optional_with_name(input, metavar_name)?,
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
fn append_if_statement(if_or_elseif: Either<&If, &ElseIf>, tail_cast: Option<&TailCast>, tokens: &mut TokenStream) {
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

    if block.expr.contains_metavars() || tail_cast.is_some() {
        // When an if's block has a metavariable, each condition must be put in a different `else-if` block.
        // The metavar_name check is done in Parse
        for cond_ty in condition {
            append_if_tokens(&mut is_first, if_token, else_token, tokens);
            Condition::single_to_tokens(cond_ty, generic_t, is_token.0.span(), tokens);
            // Clone the same body for each block.
            // Metavariables are resolved to cond_ty
            block.braces.surround(tokens, |tokens| match tail_cast {
                // If macro input contains an outer/tail cast, the transmute is done in every branch
                Some(tail_cast) => {
                    let expr = block.expr.to_token_stream(cond_ty);
                    let cast = metacast_to_token_stream(quote_spanned!(expr.span()=> __result), &tail_cast.ty, cond_ty, generic_t, MetaCastType::ConcreteToGeneric);
                    quote! {
                        // Put block in a separate local variable to prevent the entire block from being in an unsafe block
                        let __result = { #expr };
                        #cast
                    }.to_tokens(tokens)
                }, 
                None => block.expr.to_tokens(cond_ty, tokens),
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
