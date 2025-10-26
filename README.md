# Match `T`

[![Crates.io](https://img.shields.io/crates/v/match_t.svg?style=for-the-badge&logo=docsdotrs)](https://crates.io/crates/match_t)
[![Docs](https://img.shields.io/docsrs/match_t?style=for-the-badge&logo=rust)](https://docs.rs/match_t/latest)

Allows a programmer to write an `if` or `match` expression where the items being compared are **Types**.
Specifically, you compare a *generic type* (`T`) against any *concrete type* in Rust and run different code dpending on the type that matched.

## Inspiration

In a [work-in-progress crate](https://github.com/Megadash452/ez-jni-rs/),
there are a couple of traits (let's say `FromArrayObject`) that need custom implementations for primitives,
but all other types can use the blanket implementation if they implement a base trait.
It became clear that I needed a language feature like [***specialization***](https://github.com/rust-lang/rust/issues/31844).
The problem is that specialization is a *very unstable* feature (it doesn't seema *anywhere* near complete) available in the nightly compiler.
However, I want my crate to work on stable and I needed it now.

I seems that specialization is the only way to do what I needed, so I developed my own solution 😁.
