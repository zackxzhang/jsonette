/* json parsing in rust */


use std::collections::HashMap;


enum JValue {
    JNull,
    JBool(bool),
    JString(String),
    JNumber { int: i32, frac: i32, expo: i32 },
    JArray(Vec<JValue>),
    JObject(HashMap<String, JValue>),
}


static NUMBERS: &str = "0123456789";
static _NUMBERS: &str = "-0123456789";


fn to_constant(s: &str) -> Result<JValue, String> {
    match s {
        "null"  => Ok(JValue::JNull),
        "true"  => Ok(JValue::JBool(true)),
        "false" => Ok(JValue::JBool(false)),
        _       => Err(format!("not a constant: {:?}", s)),
    }
}


fn to_escape(c: char) -> Result<char, String> {
    match c {
        '\\' => Ok('\\'),
        '"'  => Ok('"'),
        '/'  => Ok('/'),
        'b'  => Ok('\x08'),
        'f'  => Ok('\x0c'),
        'n'  => Ok('\n'),
        'r'  => Ok('\r'),
        't'  => Ok('\t'),
        _    => Err(format!("not an escape: {:?}", c)),
    }
}


struct Tape<'a> {
    data: &'a str,
    cursor: usize,
}


impl<'a> Tape<'a> {

    fn new(data: &'a str) -> Self {
        Self { data, cursor: 0 }
    }

    fn look_ahead(&self) -> Option<char> {
        self.data.get(self.cursor..)?.chars().nth(1)
    }

    fn look_behind(&self) -> Option<char> {
        self.data.get(..self.cursor)?.chars().next_back()
    }

    fn read(&self) -> Option<char> {
        self.data.get(self.cursor..)?.chars().next()
    }

    fn step(&mut self) {
        if let Some(c) = self.read() {
            self.cursor += c.len_utf8();
        }
    }

    fn skip(&mut self) {
        while let Some(c) = self.read() {
            if c.is_whitespace() {
                self.step();
            } else {
                break;
            }
        }
    }

}


fn main() {

}
