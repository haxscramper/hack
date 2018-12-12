use std::fs::File;
use std::io::prelude::*;


struct Key {
    name: String,
    code: i32,
    first_repeat: i32,
    repeat_after: i32,
}

impl Default for Key {
    fn default() -> Key {
        Key {
            name: String::from("none"),
            code: 0,
            first_repeat: -1,
            repeat_after: -1,
        }
    }
}

fn main() {
let mut file = File::open("")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;

}
