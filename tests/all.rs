use match_t::match_t;
use std::any::{Any, type_name};

#[test]
fn compile_fail() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/compile_fail/*.rs");
}

#[test]
fn normal() {
    fn my_fn<T: Any>() {
        match_t! {
            if T is u8 | u32 | u64 | usize | u128 {
                println!("T is unsigned :(")
            } else if T is i8 | i32 | i64 | isize | i128 {
                println!("T is signed! :)")
            } else {
                println!("T is... something else: {}", type_name::<T>())
            }
        }
    }
    my_fn::<u8>();
    my_fn::<i8>();
}

#[test]
fn metacast() {
    fn my_fn<T: Any>(val: T) -> Option<T> {
        match_t! {
            if T is u8 | u32 | u64 | usize | u128 {
                println!("T is unsigned :( Is it 0?: {}", val $as $T == 0);
                None
            } else if T is i8 | i32 | i64 | isize | i128 {
                println!("T is signed! :)");
                Some(val $as $T)
            } else {
                println!("T is... something else: {}", type_name::<T>());
                None
            } $as Option<T>
        }
    }
    assert_eq!(my_fn::<u8>(0), None);
    assert_eq!(my_fn::<i8>(0), Some(0));
}

/// Each else-if clause in the if statement can reference a different generic type.
#[test]
fn if_multiple_metavars() {
    fn my_fn<T: Any, G: Any>() {
        match_t! {
            if T is bool | char | u8 {
                println!("T is small :(")
            } else if G is i128 | u128 {
                println!("T is BIG! :) size: {}", size_of::<$G>())
            } else {
                println!("T is... something else: {}", std::any::type_name::<T>())
            }
        };
    }
    my_fn::<bool, String>();
    my_fn::<String, u128>();
}
