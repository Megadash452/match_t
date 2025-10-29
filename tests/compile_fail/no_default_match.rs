fn main() { }

fn my_fn<T: std::any::Any>() {
    match_t::match_t! {
        match T {
            bool | char => { println!("Hi") }
        }
    }
}
