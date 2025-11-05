//! # Match `T`
//!
//! [![Github](https://img.shields.io/badge/Github-000000?logo=github)](https://github.com/Megadash452/match_t/)
//!
//! Allows a programmer to write an `if` or `match` expression where the items being compared are **Types**.
//! Specifically, you compare a *generic type* (`T`) against any *concrete type* in Rust and run different code depending on the type that matched.

mod common;
mod r#if;
mod r#match;
mod meta_tokens;

use r#if::If;
use r#match::Match;
use proc_macro2::TokenStream;
use quote::ToTokens;
use std::fmt::{Debug, Display};
use syn::{
    Token,
    parse::{Parse, ParseStream},
};

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
/// The metavariable is not available within the `else` block of the *`if` statement*,
/// or within the *default/catch-all* (`_`) arm of the *`match` statement*.
///
/// ## Metacast
///
/// Values inside the *if blocks*/*match arms* can be converted from a **concrete type** to a **generic type** (and vice versa) with the `$as <Type>` syntax.
/// The concrete type is represented by a *metavariable* `$T`.
/// The value's actual type must conform to a type containing the *concrete type* or the *generic type* (depending on which type of cast you are doing).
/// For example, if a value has type `Option<Vec<T>>`, it can only be cast to the concrete type `$as Option<Vec<$T>>`.
/// Trying to cast it to any other type will not work.
///
/// A **metacast** can also be put at the tail end of an *if*/*match statement* from convert a **concrete type** to a **generic type** for all branches or arms.
/// However, this is only allowed if all conditions use the same *generic type* (this is only applicable in *if statement*),
/// and if the statement has an *else block* or *default arm*.
/// Also, a tail cast can't cast to a **concrete type** because the cconcrete type is only available within its specific *block*/*arm*.
///
/// ## Limitations
///
/// * The given generic type `T` must be bound to the `'static` lifetime.
///   Apparently this is required by [`TypeId::of()`][std::any::TypeId::of()]
///   because types with *different lifetimes* are different types.
///   This means that [`TypeId::of()`][std::any::TypeId::of()] would return different [`TypeId`][std::any::TypeId]s for types with lifetimes.
///   For example, references `&'a str` and `&'b str` have different [`TypeId`][std::any::TypeId]s, even though they are technically the same type.
///   <br><br>
/// * The given generic type `T` can only be compared with **concrete types** (e.g. `i32`).
///   This macro can't check if `T` implements some trait because that is not possible in Rust (afaik).
///   <br><br>
/// * To assign a **concrete type** to the metavariable `$T`,
///   the macro checks each type comparison individually,
///   cloning the block for ***each type*** that `T` is compared with.
///   With this in mind, the macro will put blocks that *don't* use `$T` to optimize code size,
///   but you should still try to keep each block's size as *short as possible*.
///
/// ## Examples
///
/// The following two examples produce identical code:
///
/// With `if-else` statement:
///
/// ```
/// # use match_t::match_t;
/// # use std::any::Any;
/// # use std::any::type_name;
/// fn my_fn<T: Any>(val: T) -> Option<T> {
///     match_t! {
///         if T is u8 | u32 | u64 | usize | u128 {
///             println!("T is unsigned :( Is it 0?: {}", val $as $T == 0);
///             None
///         } else if T is i8 | i32 | i64 | isize | i128 {
///             println!("T is signed! :)");
///             Some(val $as $T)
///         } else {
///             println!("T is... something else: {}", type_name::<T>());
///             None
///         } $as Option<T>
///     }
/// }
/// ```
///
/// With `match` statement:
///
/// ```
/// # use match_t::match_t;
/// # use std::any::Any;
/// # use std::any::type_name;
/// fn my_fn<T: Any>(val: T) -> Option<T> {
///     match_t! {
///         match T {
///             u8 | u32 | u64 | usize | u128 => {
///                 println!("T is unsigned :( Is it 0?: {}", val $as $T == 0);
///                 None
///             }
///             i8 | i32 | i64 | isize | i128 => {
///                 println!("T is signed! :)");
///                 Some(val $as $T)
///             },
///             _ => {
///                 println!("T is... something else: {}", type_name::<T>());
///                 None
///             },
///         } $as Option<T>
///     }
/// }
/// ```
///
/// When using an `if` statement, each `else-if` clause can reference a different *generic type*.
///
/// ```
/// # use match_t::match_t;
/// # use std::any::Any;
/// # use std::any::type_name;
/// fn my_fn<T: Any, G: Any>() {
///     match_t! {
///         if T is G {
///             println!("T and G are the same type.")
///         } else if T is bool | char {
///             println!("T is small :(")
///         } else {
///             println!("T is {}, and G is {}.", type_name::<T>(), type_name::<G>())
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

/// Called by [`match_t`].
enum MatchT {
    If(If),
    Match(Match),
}
impl Parse for MatchT {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(if input.peek(Token![if]) {
            Self::If(input.parse::<If>()?)
        } else {
            Self::Match(input.parse::<Match>()?)
        })
    }
}
impl ToTokens for MatchT {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::If(if_t) => if_t.to_tokens(tokens),
            Self::Match(match_t) => match_t.to_tokens(tokens),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use quote::quote;
    use std::sync::LazyLock;

    #[test]
    fn match_output() {
        let match_t = syn::parse2::<Match>(quote! {
            match T {
                bool | char | u8 => println!("T is small :("),
                i128 | u128 => println!("T is BIG! :) size: {}", size_of::<$T>()),
                _ => println!("T is... something else: {}", type_name::<T>())
            }
        }).unwrap();

        assert!(compare_tokenstreams(
            match_t.to_token_stream(),
            syn::parse_str::<TokenStream>(&COMMON_OUTPUT).unwrap()
        ))
    }

    #[test]
    fn default_only_match() {
        let match_t = syn::parse2::<Match>(quote! {
            match T {
                _ => println!("T is... something else: {}", type_name::<T>())
            }
        }).unwrap();

        assert!(compare_tokenstreams(
            match_t.to_token_stream(),
            syn::parse2::<TokenStream>(quote! {
                println!("T is... something else: {}", type_name::<T>())
            }).unwrap()
        ))
    }

    #[test]
    fn no_default_match() {
        let match_t = syn::parse2::<Match>(quote! {
            match T {
                bool | char => println!("T is small :("),
                i128 | u128 => println!("T is BIG! :) size: {}", size_of::<$T>())
            }
        }).unwrap();

        assert!(compare_tokenstreams(
            match_t.to_token_stream(),
            syn::parse2::<TokenStream>(quote! {
                if ::std::any::TypeId::of::<T>() == ::std::any::TypeId::of::<bool>()
                || ::std::any::TypeId::of::<T>() == ::std::any::TypeId::of::<char>() {
                    println!("T is small :(")
                } else if ::std::any::TypeId::of::<T>() == ::std::any::TypeId::of::<i128>() {
                    println!("T is BIG! :) size: {}", size_of::<i128>())
                } else if ::std::any::TypeId::of::<T>() == ::std::any::TypeId::of::<u128>() {
                    println!("T is BIG! :) size: {}", size_of::<u128>())
                }
            }).unwrap()
        ))
    }

    #[test]
    fn if_output() {
        let if_t = syn::parse2::<If>(quote! {
            if T is bool | char | u8 {
                println!("T is small :(")
            } else if T is i128 | u128 {
                println!("T is BIG! :) size: {}", size_of::<$T>())
            } else {
                println!("T is... something else: {}", type_name::<T>())
            }
        }).unwrap();

        assert!(compare_tokenstreams(
            if_t.to_token_stream(),
            syn::parse_str::<TokenStream>(&COMMON_OUTPUT).unwrap()
        ))
    }

    #[test]
    fn short_if() {
        let if_t = syn::parse2::<If>(quote! {
            if T is bool | char {
                println!("T is small :( size:", size_of::<$T>())
            }
        }).unwrap();

        assert!(compare_tokenstreams(
            if_t.to_token_stream(),
            syn::parse2::<TokenStream>(quote! {
                if ::std::any::TypeId::of::<T>() == ::std::any::TypeId::of::<bool>() {
                    println!("T is small :( size:", size_of::<bool>())
                } else if ::std::any::TypeId::of::<T>() == ::std::any::TypeId::of::<char>() {
                    println!("T is small :( size:", size_of::<char>())
                }
            }).unwrap()
        ))
    }

    #[test]
    fn if_else() {
        let if_t = syn::parse2::<If>(quote! {
            if T is bool {
                println!("T is small :( size:", size_of::<$T>())
            } else {
                println!("T is... something else: {}", type_name::<T>())
            }
        }).unwrap();

        assert!(compare_tokenstreams(
            if_t.to_token_stream(),
            syn::parse2::<TokenStream>(quote! {
                if ::std::any::TypeId::of::<T>() == ::std::any::TypeId::of::<bool>() {
                    println!("T is small :( size:", size_of::<bool>())
                } else {
                    println!("T is... something else: {}", type_name::<T>())
                }
            }).unwrap()
        ))
    }

    #[test]
    fn metacast_if_outer() {
        let if_t = syn::parse2::<If>(quote! {
            if T is bool | char {
                [val $as $T; 5]
            } else {
                panic!("Incorrect type")
            } $as [T; 5]
        }).unwrap();

        assert!(compare_tokenstreams(
            if_t.to_token_stream(),
            syn::parse_str::<TokenStream>(&COMMON_METACAST_OUTER).unwrap()
        ))
    }

    #[test]
    fn metacast_match_outer() {
        let match_t = syn::parse2::<Match>(quote! {
            match T {
                bool | char => [val $as $T; 5],
                _ => panic!("Incorrect type")
            } $as [T; 5]
        }).unwrap();

        assert!(compare_tokenstreams(
            match_t.to_token_stream(),
            syn::parse_str::<TokenStream>(&COMMON_METACAST_OUTER).unwrap()
        ))
    }

    static COMMON_OUTPUT: LazyLock<String> = LazyLock::new(|| {
        quote! {
            if ::std::any::TypeId::of::<T>() == ::std::any::TypeId::of::<bool>()
            || ::std::any::TypeId::of::<T>() == ::std::any::TypeId::of::<char>()
            || ::std::any::TypeId::of::<T>() == ::std::any::TypeId::of::<u8>() {
                println!("T is small :(")
            } else if ::std::any::TypeId::of::<T>() == ::std::any::TypeId::of::<i128>() {
                println!("T is BIG! :) size: {}", size_of::<i128>())
            } else if ::std::any::TypeId::of::<T>() == ::std::any::TypeId::of::<u128>() {
                println!("T is BIG! :) size: {}", size_of::<u128>())
            } else {
                println!("T is... something else: {}", type_name::<T>())
            }
        }
        .to_string()
    });

    static COMMON_METACAST_OUTER: LazyLock<String> = LazyLock::new(|| {
        quote! {
            if ::std::any::TypeId::of::<T>() == ::std::any::TypeId::of::<bool>() {
                let __result = { [{
                    let __value = (val);
                    unsafe {
                        let __dst = ::core::mem::transmute::<*const T, *const bool>(&(__value) as *const _);
                        ::core::mem::forget(__value);
                        ::core::ptr::read(__dst)
                    }
                }; 5] };
                {
                    let __value = (__result);
                    unsafe {
                        let __dst = ::core::mem::transmute::<*const [bool; 5], *const [T; 5]>(&(__value) as *const _);
                        ::core::mem::forget(__value);
                        ::core::ptr::read(__dst)
                    }
                }
            } else if ::std::any::TypeId::of::<T>() == ::std::any::TypeId::of::<char>() {
                let __result = { [{
                    let __value = (val);
                    unsafe {
                        let __dst = ::core::mem::transmute::<*const T, *const char>(&(__value) as *const _);
                        ::core::mem::forget(__value);
                        ::core::ptr::read(__dst)
                    } }; 5] };
                    {
                        let __value = (__result);
                        unsafe {
                            let __dst = ::core::mem::transmute::<*const [char; 5], *const [T; 5]>(&(__value) as *const _);
                            ::core::mem::forget(__value);
                            ::core::ptr::read(__dst)
                        }
                    }
            } else {
                panic!("Incorrect type")
            }
        }
        .to_string()
    });

    fn compare_tokenstreams(stream1: TokenStream, stream2: TokenStream) -> bool {
        let mut stream1 = stream1.into_iter();
        let mut stream2 = stream2.into_iter();

        while let Some((tt1, tt2)) = stream1.next().zip(stream2.next()) {
            use proc_macro2::TokenTree;

            match (tt1, tt2) {
                (TokenTree::Ident(ident1), TokenTree::Ident(ident2))
                    if ident1.to_string() == ident2.to_string() => {}
                (TokenTree::Literal(lit1), TokenTree::Literal(lit2))
                    if lit1.to_string() == lit2.to_string() => {}
                (TokenTree::Punct(punct1), TokenTree::Punct(punct2))
                    if punct1.as_char() == punct2.as_char() => {}
                (TokenTree::Group(group1), TokenTree::Group(group2)) => {
                    if !compare_tokenstreams(group1.stream(), group2.stream()) {
                        return false;
                    }
                }
                _ => return false,
            }
        }

        // If either of th eiterators were not consumed, the tokenstreams have different length, therefore not equal.
        if stream1.next().is_some() || stream2.next().is_some() {
            return false;
        }

        true
    }
}
