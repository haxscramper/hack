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
                let mut key: Key =
                    self.keymap[*logic_row][*logic_col].clone();
                if !key.is_valid {
                    continue;
                }

                let mut press_count: i32 =
                    *self.pressed.get(&key).unwrap();
                // TODO Read GPIO state
                let mut gpio_high = false;

                if gpio_high
                // Button is pressed
                {
                    press_count = press_count + 1;
                    if press_count == 0
                    // For the first time
                    {
                        key.state_change =
                            Some(StateChange::Changedpressed);
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

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
enum ModifierKeys {
    Ctrl,
    Meta,
    Shift,
    Super,
    Hyper,
}

#[derive(Clone, Debug, PartialEq)]
struct PressedModifiers {
    Shift: bool,
    Ctrl: bool,
    Meta: bool,
    Super: bool,
    Hyper: bool,
}

impl PressedModifiers {
    fn new() -> PressedModifiers {
        PressedModifiers {
            Shift: false,
            Ctrl: false,
            Meta: false,
            Super: false,
            Hyper: false,
        }
    }
}

struct ActionResolver {
    /// Action assigned to each key on the keypad:
    actions: HashMap<(usize, usize), Action>,
    /// Set of modifier keys pressed on the keypad
    pressedModifiers: HashMap<(usize, usize), ModifierKeys>,
    /// Set of modifier keys pressed outisde of the keypad
    external_modifiers: PressedModifiers,
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

    fn set_external_modifiers(&mut self, modifiers: &PressedModifiers) {
        self.external_modifiers = modifiers.clone();
    }

    fn process_keys(&mut self, keys: Vec<Key>) {
        for phys_key in keys {
            let action =
                self.actions.get(&(phys_key.row, phys_key.column));
            if action.is_some() {
                if let Action::Key(key) = action.unwrap() {
                    if key.switchModifier.is_some() {
                        println!("Pressed key");
                    }
                }

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
            external_modifiers: PressedModifiers::new(),
        }
    }

    /// Get modifier keys *including* external modifiers
    fn get_modifiers(&self) -> PressedModifiers {
        let mut res = self.external_modifiers.clone();
        for (pos, val) in &self.pressedModifiers {
            match val {
                ModifierKeys::Ctrl => res.Ctrl = true,
                ModifierKeys::Shift => res.Shift = true,
                ModifierKeys::Meta => res.Meta = true,
                ModifierKeys::Super => res.Super = true,
                ModifierKeys::Hyper => res.Hyper = true,
            }
        }

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
    let mut central_keypad = Keypad::new();
    let mut numpad_keypad = Keypad::new();
    let mut central_resolver = ActionResolver::new();
    let mut keypad_resolver = ActionResolver::new();

    central_keypad.mapping_from_jsom("settings/main_area_convertion.json");
    numpad_keypad.mapping_from_jsom("settings/numpad_conversion.json");
    central_resolver.config_from_json("settings/main_area_keymap.json");
    keypad_resolver.config_from_json("settings/keypad_keymap.json");

    {
        // Read all changed keys, without looking at what they actually are
        let mut central_changed = central_keypad.scan();
        let mut numpad_changed = numpad_keypad.scan();

        // Process all keys in the main area, find modifier keys that are pressed
        central_resolver.process_keys(central_changed);
        let modifier_keys = central_resolver.get_modifiers();

        // Process all keys in numpad area, use local modifier keys and
        // modifers pressed in previous area
        keypad_resolver.set_external_modifiers(&modifier_keys);
        keypad_resolver.process_keys(numpad_changed);
        let modifier_keys = keypad_resolver.get_modifiers();
    }

    println!("Finished reading config");

    Ok(())
}
