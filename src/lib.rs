// TODO: document crate
use proc_macro2::{Punct, Span, TokenStream};
use quote::{ToTokens, TokenStreamExt as _, quote, quote_spanned};
use syn::{
    Block, Expr, Ident, Token, Type, braced,
    parse::{Parse, ParseStream, discouraged::Speculative as _},
    punctuated::Punctuated,
    spanned::Spanned as _,
    token::Brace,
};

/// Run different of code depending on the **concrete type** of `T`.
///
/// This is essentially an `if/match` statement for *generic types*.
/// Callers can use `match` with `T` as the *input* and the *cases* are **concrete types**.
/// Callers can also use `if-else` statements with the `is` meta-keyword.
///
/// Blocks of code are given the metavariable `$T`,
/// which resolves to the
///
/// ## Limitations
///
/// * The given type `T` can only be compared with **concrete types** (e.g. `i32`).
///   This macro can't check if `T` implements some trait because that is not possible in Rust (afaik).
///   <br><br>
/// * The `$T` metavariable can't be used within `else` blocks or *default case* (`_` in match).
///   To assign a *type* to the metavariable, the macro checks each type comparison individually,
///   cloning the block for ***each type*** that `T` is compared with.
///
///   With this in mind, the macro will put blocks that *don't* use `$T` to optimize code size,
///   but you should still try to keep each block's size as *short as possible*.
///
/// ## Example
///
/// The following two examples output identical code:
///
/// With `if-else` statement:
/// ```
/// # use match_t::match_t;
/// match_t! {
///     if T is bool | char | u8 | u32 | u64 | usize | u128 {
///         println!("T is unsigned!")
///     } else if T is i8 | i32 | i64 | isize | i128 {
///         println!("T is signed!")
///     } else {
///         println!("T is... something else: {}", std::any::type_name::<$T>())
///     }
/// }
/// ```
///
/// With `match` statement:
/// ```
/// # use match_t::match_t;
/// match_t! {
///     match T {
///         bool | char | u8 | u32 | u64 | usize | u128 => println!("T is unsigned!"),
///         i8 | i32 | i64 | isize | i128 => println!("T is signed!"),
///         _ => println!("T is... something else: {}", std::any::type_name::<$T>()),
///     }
/// }
/// ```
#[proc_macro]
pub fn match_t(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    match syn::parse::<MatchT>(input) {
        Ok(t) => t.into_token_stream().into(),
        Err(error) => error.into_compile_error().into(),
    }
}

// TODO: how to get the `$T` metavariable

/// Called by [`match_t`].
enum MatchT {
    If(If),
    Match(Match),
}
impl Parse for MatchT {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input
            .parse::<If>()
            .map(Self::If)
            .or_else(|_| input.parse::<Match>().map(Self::Match))
    }
}
impl ToTokens for MatchT {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::If(ift) => ift.to_tokens(tokens),
            Self::Match(matcht) => matcht.to_tokens(tokens),
        }
    }
}

struct If {
    if_token: Token![if],
    t: Type,
    is_token: IsToken,
    condition: Condition,
    block: Block,
    else_ifs: Vec<ElseIf>,
    else_stmnt: Option<Else>,
}
struct IsToken(Ident);
struct ElseIf {
    else_token: Token![else],
    if_token: Token![if],
    t: Type,
    is_token: IsToken,
    condition: Condition,
    block: Block,
}
struct Else {
    else_token: Token![else],
    block: Block,
    // Don't embed else-ifs here (make different type ElseIf) to avoid recursion.
}
impl Parse for If {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let if_token = input.parse()?;
        let t = input.parse()?;
        let is_token = input.parse()?;
        let condition = input.parse()?;
        let block = input.parse()?;

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
                        Some(_) => {
                            return Err(syn::Error::new(
                                new.else_token.span,
                                "Two 'else' statements are not allowed",
                            ));
                        }
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
impl Parse for ElseIf {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            else_token: input.parse()?,
            if_token: input.parse()?,
            t: input.parse()?,
            is_token: input.parse()?,
            condition: input.parse()?,
            block: input.parse()?,
        })
    }
}
impl Parse for Else {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            else_token: input.parse()?,
            block: input.parse()?,
        })
    }
}
impl ToTokens for If {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.if_token.to_tokens(tokens);
        self.condition
            .to_tokens(&self.t, self.is_token.0.span(), tokens);
        self.block.to_tokens(tokens);

        for else_if in &self.else_ifs {
            else_if.else_token.to_tokens(tokens);
            else_if.if_token.to_tokens(tokens);
            else_if
                .condition
                .to_tokens(&else_if.t, else_if.is_token.0.span(), tokens);
            else_if.block.to_tokens(tokens);
        }

        if let Some(else_stmnt) = &self.else_stmnt {
            else_stmnt.else_token.to_tokens(tokens);
            else_stmnt.block.to_tokens(tokens);
        }
    }
}

struct Match {
    match_token: Token![match],
    t: Type,
    braces: Brace,
    arms: Vec<MatchArm>,
    default_case_arm: Option<(Token![=>], MatchBody)>,
}
struct MatchArm {
    case: Condition,
    arrow_token: Token![=>],
    body: MatchBody,
}
enum MatchBody {
    Block {
        block: Block,
        /// Match arm with braces can **skip** comma.
        comma: Option<Token![,]>,
    },
    Expr {
        expr: Expr,
        /// Match arm with braces **requires** a comma.
        comma: Token![,],
    },
}
impl Parse for Match {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let match_token = input.parse()?;
        let t = input.parse()?;
        let match_body;
        let braces = braced!(match_body in input);
        let mut arms = Vec::new();
        let mut default_case_arm = None;

        while !match_body.is_empty() {
            // When the case arm is just `_`, that goes in the default_case_arm.
            let fork = match_body.fork();
            if let Ok(ident) = fork.parse::<Ident>() && ident == "_" {
                match_body.advance_to(&fork);

                default_case_arm = Some((match_body.parse()?, match_body.parse()?));

                // default_case must also be the LAST case
                if !match_body.is_empty() {
                    return Err(match_body
                        .error("Unexpected tokens: Default case must be the last case"));
                }
            }
            // Parse normal case
            arms.push(match_body.parse()?);
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
impl Parse for MatchArm {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            case: input.parse()?,
            arrow_token: input.parse()?,
            body: input.parse()?,
        })
    }
}
impl Parse for MatchBody {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Brace) {
            Ok(Self::Block {
                block: input.parse()?,
                comma: input.parse()?,
            })
        } else {
            Ok(Self::Expr {
                expr: input.parse()?,
                comma: input.parse()?,
            })
        }
    }
}
impl MatchBody {
    fn to_tokens(&self, default_braces: &Brace, tokens: &mut TokenStream) {
        match self {
            Self::Block { block, comma } => {
                block.to_tokens(tokens);
                comma.to_tokens(tokens);
            }
            Self::Expr { expr, comma } => {
                // If statement block must have braces, so add them if they are not in the arm's body.
                default_braces.surround(tokens, |tokens| expr.to_tokens(tokens));
                comma.to_tokens(tokens);
            }
        }
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
            default_case_arm.1.to_tokens(&self.braces, tokens);
        }
    }
}

struct Condition(Punctuated<Type, Token![|]>);
impl Condition {
    /// Generate a `boolean` [`Condition`] that compares `T` with all [`Types`][Type] in `Self` for use in an `if` statement.
    ///
    /// **eq_span** is the [`Span`] that will be assigned to the *equal signs* (`==`) in the condition.
    fn to_tokens(&self, generic_t: &Type, eq_span: Span, tokens: &mut TokenStream) {
        let type_of_t = type_of_tokens(generic_t);
        let eq = {
            let mut first = Punct::new('=', proc_macro2::Spacing::Joint);
            let mut second = Punct::new('=', proc_macro2::Spacing::Alone);
            first.set_span(eq_span);
            second.set_span(eq_span);
            quote!(#first #second)
        };

        for pair in self.0.pairs() {
            let type_of_ty = type_of_tokens(pair.value());
            let or = pair
                .punct() // Will output no tokens when None (aka at end)
                .map(|bitor| {
                    let mut first = Punct::new('|', proc_macro2::Spacing::Joint);
                    let mut second = Punct::new('|', proc_macro2::Spacing::Alone);
                    first.set_span(bitor.span());
                    second.set_span(bitor.span());
                    quote!(#first #second)
                });

            tokens.append_all(quote! { #type_of_t #eq #type_of_ty #or });
        }
    }
}
impl Parse for Condition {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self(Punctuated::parse_separated_nonempty(input)?))
    }
}

fn type_of_tokens(ty: &Type) -> TokenStream {
    quote_spanned! {ty.span()=> ::std::any::TypeId::of::<#ty>() }
}

// TODO: test output
