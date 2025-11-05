fn main() { }

fn my_fn<T: std::any::Any>(val: T) {
    let arr = match_t::match_t! {
        if T is bool | char {
            [val $as $T; 5]
        } $as [T; 5]
    };
}
