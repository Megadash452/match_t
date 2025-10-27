#![doc = include_str!("../README.md")]

mod common;
mod r#if;
mod r#match;

use r#if::If;
use r#match::Match;
use proc_macro2::TokenStream;
use quote::ToTokens;
use std::fmt::{Debug, Display};
use syn::parse::{Parse, ParseStream};

/// Run different of code depending on the **concrete type** of `T`.
///
/// This is essentially an `if/match` statement for *generic types*.
/// Callers can use `match` with `T` as the *input* and the *cases* are **concrete types**.
/// Callers can also use `if-else` statements with the `is` meta-keyword.
/// 
/// ## Metavariable
///
/// Blocks of code are given the metavariable `$T`.
/// The `T` in this case is the same *generic type* that was passed in.
/// The metavariable resolves to the **concrete type** that matched with the type of `T`.
/// 
/// The metavariable is not available within the `else` block of the *`if`` statement*,
/// or within the *default/catch-all* (`_`) arm of the *`match` statement*.
///
/// ## Limitations
///
/// * The given type `T` can only be compared with **concrete types** (e.g. `i32`).
///   This macro can't check if `T` implements some trait because that is not possible in Rust (afaik).
///   <br><br>
/// * To assign a **concrete type** to the metavariable `$T`,
///   the macro checks each type comparison individually,
///   cloning the block for ***each type*** that `T` is compared with.
///   With this in mind, the macro will put blocks that *don't* use `$T` to optimize code size,
///   but you should still try to keep each block's size as *short as possible*.
///
/// ## Example
///
/// The following two examples produce identical code:
///
/// With `if-else` statement:
/// ```
/// # use match_t::match_t;
/// fn my_fn<T: Any>() {
///     match_t! {
///         if T is bool | char | u8 | u32 | u64 | usize | u128 {
///             println!("T is unsigned :(")
///         } else if T is i8 | i32 | i64 | isize | i128 {
///             println!("T is signed! :) Absolute value of -6: {}", $T::abs(-6))
///         } else {
///             println!("T is... something else: {}", std::any::type_name::<T>())
///         }
///     }
/// }
/// ```
///
/// With `match` statement:
/// ```
/// # use match_t::match_t;
/// fn my_fn<T: Any>() {
///     match_t! {
///         match T {
///             bool | char | u8 | u32 | u64 | usize | u128 => println!("T is unsigned :("),
///             i8 | i32 | i64 | isize | i128 => { println!("T is signed! :) Absolute value of -6: {}", $T::abs(-6)) }
///             _ => println!("T is... something else: {}", std::any::type_name::<T>())
///         }
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

// TODO: test output
