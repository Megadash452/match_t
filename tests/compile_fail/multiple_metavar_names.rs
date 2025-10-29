fn main() { }

fn my_fn<T: std::any::Any>() {
    match_t::match_t! {
        if T is bool | char {
            println!("{}", core::mem::size_of::<$T>());
            println!("{}", core::mem::size_of::<$G>());
        } else {
            println!("{}", std::any::type_name::<T>());
        }
    }
}
