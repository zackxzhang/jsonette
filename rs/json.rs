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

impl From<JNull> for JValue {
    fn from(v: JNull) -> Self {
        JValue::Null(v)
    }
}

impl From<JBool> for JValue {
    fn from(v: JBool) -> Self {
        JValue::Bool(v)
    }
}

impl From<JString> for JValue {
    fn from(v: JString) -> Self {
        JValue::String(v)
    }
}

impl From<JNumber> for JValue {
    fn from(v: JNumber) -> Self {
        JValue::Number(v)
    }
}

impl From<JArray> for JValue {
    fn from(v: JArray) -> Self {
        JValue::Array(v)
    }
}

impl From<JObject> for JValue {
    fn from(v: JObject) -> Self {
        JValue::Object(v)
    }
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


struct Parser;

impl Parser {

    fn read_null(&self, tape: &mut Tape) -> Result<JNull, String> {
        let i = tape.i;
        if "null" == &tape[i..i+4] {
            tape.step(4);
            Ok(JNull)
        } else {
            Err("not a bool".into())
        }
    }

    fn read_bool(&self, tape: &mut Tape) -> Result<JBool, String> {
        let i = tape.i;
        match &tape[i..i+5] {
            t if t.starts_with("true") => {
                tape.step(4);
                Ok(JBool(true))
            }
            t if t.starts_with("false") => {
                tape.step(5);
                Ok(JBool(false))
            }
            _ => {
                Err("not a bool".into())
            }
        }
    }

    fn read_number(&self, tape: &mut Tape) -> Result<JNumber, String> {
        let start = tape.i;
        let mut fraction = false;
        let mut exponent = false;
        let Some(c) = tape.read() else {
            return Err("not a number".into());
        };
        if c == '0' {
            match tape.look_ahead() {
                Some(a) if NUMBERS.contains(a) => return Err("not a number".into()),
                Some(_) | None => (),
            }
        } else if _NUMBERS.contains(c) {
            ()
        } else {
            return Err("not a number".into())
        }
        tape.step(1);
        loop {
            let Some(c) = tape.read() else {
                break;
            };
            if NUMBERS.contains(c) {
                ()
            } else if c == '.' {
                if fraction || exponent ||
                tape.look_behind().is_some_and(|b| !NUMBERS.contains(b)) {
                    return Err("invalid decimal point".into());
                }
                fraction = true;
            } else if "eE".contains(c) {
                if exponent ||
                tape.look_behind().is_some_and(|b| !NUMBERS.contains(b)) {
                    return Err("invalid exponent".into());
                }
                exponent = true;
            } else if "+-".contains(c) {
                if tape.look_behind().is_some_and(|b| !"eE".contains(b)) {
                    return Err("sign must follow exponent".into());
                }
            } else {
                break;
            }
            tape.step(1);
        }
        let end = tape.i;
        if fraction || exponent {
            match tape[start..end].parse::<f64>() {
                Ok(number) => Ok(JNumber::Float(number)),
                Err(e) => Err(format!("failed to parse float: '{e}'")),
            }
        } else {
            match tape[start..end].parse::<i64>() {
                Ok(number) => Ok(JNumber::Int(number)),
                Err(e) => Err(format!("failed to parse integer: '{e}'")),
            }
        }
    }

    fn read_string(&self, tape: &mut Tape) -> Result<JString, String> {
        let mut string = String::new();
        match tape.read() {
            Some('"') => { tape.step(1); },
            Some(_) | None => return Err("not an string".into()),
        }
        loop {
            let Some(c) = tape.read() else {
                return Err("string not complete".into())
            };
            if c == '"' {
                break;
            } else if c == '\\' {
                let Some(d) = tape.read() else {
                    return Err("bad escape".into());
                };
                if r#""\/bfnrt"#.contains(d) {
                    tape.step(1);
                    string.push(to_escape(d)?);
                } else if d == 'u' {
                    tape.step(1);
                    let i = tape.i;
                    match u32::from_str_radix(&tape[i..i+4], 16)
                        .ok().and_then(char::from_u32) {
                        Some(ch) => string.push(ch),
                        None => return Err("bad hexadecimals".into()),
                    }
                    tape.step(4);
                }
            } else {
                string.push(c);
            }
        }
        Ok(JString(string))
    }

    fn read_array(&self, tape: &mut Tape) -> Result<JArray, String> {
        let mut array: Vec<JValue> = Vec::new();
        match tape.read() {
            Some('[') => { tape.step(1); tape.skip(); },
            Some(_) | None => return Err("not an array".into()),
        }
        match tape.read() {
            Some(']') => { tape.step(1); return Ok(JArray(array)); },
            Some(_) => (),
            None => return Err("array not complete".into()),
        }
        loop {
            tape.skip();
            let value = self.read_value(tape);
            array.push(value?);
            tape.skip();
            match tape.read() {
                Some(',') => { tape.step(1); },
                Some(']') => { tape.step(1); break; },
                Some(_) | None => return Err("array missing , or ]".into()),
            }
        }
        Ok(JArray(array))
    }

    fn read_object(&self, tape: &mut Tape) -> Result<JObject, String> {
        let mut object: HashMap<String, JValue> = HashMap::new();
        match tape.read() {
            Some('{') => { tape.step(1); tape.skip(); },
            Some(_) | None => return Err("not an object".into()),
        }
        match tape.read() {
            Some('}') => { tape.step(1); return Ok(JObject(object)); },
            Some(_) => (),
            None => return Err("object not complete".into()),
        }
        loop {
            tape.skip();
            let key = self.read_string(tape);
            tape.skip();
            match tape.read() {
                Some(':') => { tape.step(1); },
                Some(_) | None => return Err("object missing :".into()),
            }
            tape.skip();
            let value = self.read_value(tape);
            object.insert(key?.0, value?);
            tape.skip();
            match tape.read() {
                Some(',') => { tape.step(1); },
                Some('}') => { tape.step(1); break; },
                Some(_) | None => return Err("object missing , or }".into()),
            }
        }
        Ok(JObject(object))
    }

    fn read_value(&self, tape: &mut Tape) -> Result<JValue, String> {
        let c = match tape.read() {
            Some('"') => return Ok(self.read_string(tape)?.into()),
            Some('[') => return Ok(self.read_array(tape)?.into()),
            Some('{') => return Ok(self.read_object(tape)?.into()),
            Some(c) => c,
            None => return Err("bad value".into()),
        };
        if c == 'n' {
            Ok(self.read_null(tape)?.into())
        } else if "tf".contains(c) {
            Ok(self.read_bool(tape)?.into())
        } else if _NUMBERS.contains(c) {
            Ok(self.read_number(tape)?.into())
        } else {
            Err("bad value".into())
        }
    }

    fn read(&self, data: &str) -> Result<JValue, String> {
        let mut tape = Tape::new(data);
        tape.skip();
        let value = self.read_value(&mut tape);
        tape.skip();
        match tape.read() {
            Some(_) => Err("bad remainder".into()),
            None => value
        }
    }
}


fn main() {

}
