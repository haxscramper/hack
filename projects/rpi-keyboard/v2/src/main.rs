use std::collections::HashMap;
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
extern crate json;

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
enum StateChange {
    /// Previously unmodifier now pressed
    Changedpressed,
    /// Previously pressed now released
    Changedreleased,
    /// Previously pressed now repeating
    Changedrepeat,
    /// Previously pressed no change now
    Idlepressed,
    /// Not pressed no change
    Idlereleased,
}

#[derive(Debug, Eq, Hash, Clone)]
struct Key {
    first_repeat: Option<i32>,
    repeat_after: Option<i32>,
    state_change: Option<StateChange>,
    is_valid: bool,
}

impl PartialEq for Key {
    fn eq(&self, other: &Key) -> bool {
        self.first_repeat == other.first_repeat && self.repeat_after == other.repeat_after
    }
}

impl Default for Key {
    fn default() -> Key {
        Key {
            first_repeat: None,
            repeat_after: None,
            state_change: None,
            is_valid: false,
        }
    }
}

#[derive(Debug)]
struct Keypad {
    /// Mapping between logical and physical rows
    rows: HashMap<usize, i32>,
    /// Mapping between logical and physical columns
    cols: HashMap<usize, i32>,
    /// Logical layout of keys on the keypad. I.e where each key is
    /// positioned (Ex: on default keyboard esc will be placed at [0][0])
    keymap: Vec<Vec<Key>>,
    /// Set of keys that were already pressed during one of the previous
    /// scans and amount of scans that thay have sustained this state
    pressed: HashMap<Key, i32>,
}

impl Default for Keypad {
    fn default() -> Keypad {
        Keypad {
            rows: HashMap::new(),
            cols: HashMap::new(),
            keymap: Vec::new(),
            pressed: HashMap::new(),
        }
    }
}

impl Keypad {
    /// Get vector pf keys that have changed state between scans
    fn scan(&mut self) -> Vec<Key> {
        let mut res = Vec::<Key>::new();

        for (logic_col, physic_col) in &self.cols {
            // TODO Set column at hight output
            for (logic_row, physic_row) in &self.rows {
                let mut key: Key = self.keymap[*logic_row][*logic_col].clone();
                if !key.is_valid {
                    continue;
                }

                let mut press_count: i32 = *self.pressed.get(&key).unwrap();
                // TODO Read GPIO state
                let mut gpio_high = false;

                if gpio_high
                // Button is pressed
                {
                    press_count = press_count + 1;
                    if press_count == 0
                    // For the first time
                    {
                        key.state_change = Some(StateChange::Changedpressed);
                    } else if key.first_repeat.is_some() && press_count == key.first_repeat.unwrap()
                    // Continue press
                    {
                        key.state_change = Some(StateChange::Changedrepeat);
                    }
                } else {
                    press_count = 0;
                    key.state_change = Some(StateChange::Changedreleased);
                }

                res.push(key.clone());

                if key.first_repeat.is_some()
                    && key.repeat_after.is_some()
                    && press_count >= key.first_repeat.unwrap() + key.repeat_after.unwrap()
                // Press count reached maximum, repeating press on
                // the next run
                {
                    press_count = key.first_repeat.unwrap();
                }

                self.pressed.insert(key, press_count);
            }
        }

        // TODO Insert newly created keys instead of rewriting whole map
        // TODO Remove released keys from already pressed
        // TODO Set changed states to returned keys
        return res;
    }
}

fn json_from_file(path: &str) -> Result<json::JsonValue, Box<Error>> {
    let mut file: File = File::open(path)?;
    let mut contents: String = String::new();
    file.read_to_string(&mut contents)?;

    Ok(json::parse(&contents)?)
}

fn main() -> std::io::Result<()> {
    let parsed = json_from_file("../../settings/keymap.json");
    let rows: HashMap<usize, i32> = HashMap::new();
    let cols: HashMap<usize, i32> = HashMap::new();
    let keymap: Vec<Vec<Key>> = Vec::new();

    let mut keypad: Keypad = Keypad {
        cols: cols,
        rows: rows,
        keymap: keymap,
        pressed: HashMap::new(),
    };

    let mut looping: bool = true;
    while looping {
        // Get all keys that changed state
        let pressed_keys: Vec<Key> = keypad.scan();
        // Execute actions based on key state change
    }

    Ok(())
}
