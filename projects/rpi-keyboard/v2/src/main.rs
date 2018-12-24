use std::collections::HashMap;
use std::collections::HashSet;
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
    row: usize,
    column: usize,
    state_change: Option<StateChange>,
    is_valid: bool,
    repetitions: Option<Vec<i32>>,
}

impl PartialEq for Key {
    fn eq(&self, other: &Key) -> bool {
        self.row == other.row && self.column == other.column
    }
}

impl Key {
    fn default(_row: usize, _col: usize) -> Key {
        Key {
            row: _row,
            column: _col,
            repetitions: None,
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
                    }
                // TODO Repeat keys
                } else {
                    press_count = 0;
                    key.state_change = Some(StateChange::Changedreleased);
                }

                res.push(key.clone());

                // TODO Repeat keys

                self.pressed.insert(key, press_count);
            }
        }

        // TODO Insert newly created keys instead of rewriting whole map
        // TODO Remove released keys from already pressed
        // TODO Set changed states to returned keys
        return res;
    }

    fn new() -> Keypad {
        Keypad {
            keymap: Vec::new(),
            pressed: HashMap::new(),
            cols: HashMap::new(),
            rows: HashMap::new(),
        }
    }

    fn mapping_from_jsom(&mut self, path: &str) -> Result<(), Box<Error>> {
        let config: json::JsonValue = json_from_file(path)?;

        Ok(())
    }
}

struct KeyAction {
    /// Textual name of the key
    name: String,
    /// Default keyboard key code
    defaultCode: i32,
    /// Key code to send if shift is pressed. Other modifer keys should be relased
    onShiftCode: Option<i32>,
    /// Key code to send if ctrl is pressed.  Other modifer keys should be relased
    onCtrlCode: Option<i32>,
    /// Key code to send if meta is pressed.  Other modifer keys should be relased
    onMetaCode: Option<i32>,
    /// Key code to send if super is pressed. Other modifer keys should be relased
    onSuperCode: Option<i32>,
    /// Which modifier will be enabled when key is pressed
    switchModifier: Option<ModifierKeys>,
}

struct ScriptAction {
    // TODO implement
}

enum Action {
    Key(KeyAction),
    Script(ScriptAction),
}

enum ModifierKeys {
    Ctrl,
    Meta,
    Shift,
    Super,
    Hyper,
}

struct ActionResolver {
    actions: HashMap<(usize, usize), Action>,
    pressedModifiers: HashMap<(usize, usize), ModifierKeys>,
}

impl ActionResolver {
    fn config_from_json(&mut self, path: &str) -> Result<(), Box<Error>> {
        let config: json::JsonValue = json_from_file(path)?;

        let mappings = &config["mappings"];
        for (row, val) in mappings.entries() {
            for (col, val) in mappings[row].entries() {
                println!("{} {} maps to {}", row, col, val);
            }
        }

        Ok(())
    }

    fn processKeys(&mut self, keys: Vec<Key>) {
        for key in keys {
            let action = self.actions.get(&(key.row, key.column));
            if (action.is_some()) {
                let action = action.unwrap();
                // TODO Check keys is modifier key
            }
        }
        // * Find switched modifier keys in new keys

        // * If modifier key has been released check if
        //   it is not present and then turn it off
        // * For all input keys perform required actions
    }

    fn new() -> ActionResolver {
        ActionResolver {
            actions: HashMap::new(),
            pressedModifiers: HashMap::new(),
        }
    }
}

fn json_from_file(path: &str) -> Result<json::JsonValue, Box<Error>> {
    let mut file: File = File::open(path)?;
    let mut contents: String = String::new();
    file.read_to_string(&mut contents)?;

    Ok(json::parse(&contents)?)
}

fn main() -> std::io::Result<()> {
    let mut central = Keypad::new();
    let mut keypad = Keypad::new();
    let mut centralResolver = ActionResolver::new();

    central.mapping_from_jsom("settings/main_area_convertion.json");
    keypad.mapping_from_jsom("settings/numpad_conversion.json");
    centralResolver.config_from_json("settings/main_area_keymap.json");

    println!("Finished reading config");

    Ok(())
}
