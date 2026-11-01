/* json parsing in rust */


use std::collections::HashMap;

struct JNull;
struct JBool(bool);
struct JString(String);
enum JNumber {
    Int(i64),
    Float(f64),
}
struct JArray(Vec<JValue>);
struct JObject(HashMap<String, JValue>);
enum JValue {
    Null(JNull),
    Bool(JBool),
    String(JString),
    Number(JNumber),
    Array(JArray),
    Object(JObject),
}


static NUMBERS: &str = "0123456789";
static _NUMBERS: &str = "-0123456789";


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
    i: usize,
}

impl<'a> Tape<'a> {

    fn new(data: &'a str) -> Self {
        Self { data, i: 0 }
    }

    fn look_ahead(&self) -> Option<char> {
        self.data.get(self.i..)?.chars().nth(1)
    }

    fn look_behind(&self) -> Option<char> {
        self.data.get(..self.i)?.chars().next_back()
    }

    fn read(&self) -> Option<char> {
        self.data.get(self.i..)?.chars().next()
    }

    fn step(&mut self, k: usize) {
        self.i += k;
    }

    fn skip(&mut self) {
        while let Some(c) = self.read() {
            if c.is_whitespace() {
                self.step(1);
            } else {
                break;
            }
        }
    }

}

use std::ops::{Index, Range, RangeFull};

impl<'a> Index<Range<usize>> for Tape<'a> {
    type Output = str;
    fn index(&self, range: Range<usize>) -> &str {
        &self.data[range]
    }
}

impl<'a> Index<RangeFull> for Tape<'a> {
    type Output = str;
    fn index(&self, _range: RangeFull) -> &str {
        &self.data[..]
    }
}


fn main() {

}
