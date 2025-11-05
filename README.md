# Match `T`

[![Crates.io](https://img.shields.io/crates/v/match_t.svg?style=for-the-badge&logo=docsdotrs)](https://crates.io/crates/match_t)
[![Docs](https://img.shields.io/docsrs/match_t?style=for-the-badge&logo=rust)](https://docs.rs/match_t/latest)

Allows a programmer to write an `if` or `match` expression where the items being compared are **Types**.
Specifically, you compare a *generic type* (`T`) against any *concrete type* in Rust and run different code depending on the type that matched.

## Usage

Run `cargo add match_t` within your project's directory.

Or, put this line in your project's `Cargo.toml` file (under **dependencies**:):
`match_t = "0.2.0"`.

## Examples

This example shows the macro using an `if` statement:

```rust
match_t! {
    if T is bool | char | u8 {
        println!("T is small :(")
    } else if T is i128 | u128 {
        println!("T is BIG! :) size: {}", size_of::<$T>())
    } else {
        println!("T is... something else: {}", type_name::<T>())
    }
}
```

This example shows the macro using a `match` statement:

```rust
match_t! {
    match T {
        bool | char | u8 => println!("T is small :("),
        i128 | u128 => println!("T is BIG! :) size: {}", size_of::<$T>()),
        _ => println!("T is... something else: {}", type_name::<T>())
    }
}
```

Both examples above are *equivalent* and resolve to the following tokens:

```rust
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
```

## Metavariables and MetaCasts

Each branch makes available the **concrete type** it reolved to with this token: `$T`.

Values of the *generic type* (or types that contain the *generic type*) can be cast to the **concrete type** and vice versa.
A cast to the **concrete type** has a *metavariable* `$T` in the type,
and a cast to the **generic type** has the generic `T` in the type.

For example:

```rust
fn my_fn<T: Any>(val: T) -> Option<T> {
    match_t! {
        match T {
            u8 | i8 | u32 |i32 => Some(val as $T),
            _ => None
        } as Option<T>
    }
}
```

Note in the above example that `T: Any`.
This is because [`TypeId::of()`](https://doc.rust-lang.org/std/any/struct.TypeId.html#method.of) requires the type to be `'static`.

## Inspiration

In a [work-in-progress crate](https://github.com/Megadash452/ez-jni-rs/),
there are a couple of traits (let's say `FromArrayObject`) that need custom implementations for primitives,
but all other types can use the blanket implementation if they implement a base trait.
It became clear that I needed a language feature like [***specialization***](https://github.com/rust-lang/rust/issues/31844).
The problem is that specialization is a *very unstable* feature (it doesn't seema *anywhere* near complete) available in the nightly compiler.
However, I want my crate to work on stable and I needed it now.

It seems that specialization is the only way to do what I needed, so I developed my own solution 😁.
