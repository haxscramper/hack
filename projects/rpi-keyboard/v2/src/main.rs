use std::fs::File;
use std::io::prelude::*;
extern crate json;

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

fn main() -> std::io::Result<()> {
    let mut file: File = File::open("")?;
    let mut contents: String = String::new();
    file.read_to_string(&mut contents)?;
    let parsed = json::parse(&contents);

    // let mappings = parsed["mappings"]

    // for row in mappings {
    //     println!("Hello");
    // }

    return Ok(());
}
