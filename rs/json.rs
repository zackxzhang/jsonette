/* json parsing in rust */


use std::collections::HashMap;
use std::fmt;

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

impl fmt::Display for JNull {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "null")
    }
}

impl fmt::Display for JBool {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let JBool(b) = self;
        write!(f, "{}", b)
    }
}

impl fmt::Display for JString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let JString(s) = self;
        write!(f, "{:?}", s)
    }
}

impl fmt::Display for JNumber {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            JNumber::Int(i) => write!(f, "{}", i),
            JNumber::Float(d) => write!(f, "{}", d),
        }
    }
}

impl fmt::Display for JArray {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let JArray(arr) = self;
        let display = arr.iter()
            .map(|item| item.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "[{}]", display)
    }
}

impl fmt::Display for JObject {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let JObject(obj) = self;
        let display = obj.iter()
            .map(|(key, value)| format!("{}: {}", key, value))
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "{{{}}}", display)
    }
}

impl fmt::Display for JValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            JValue::Null(n) => write!(f, "{}", n),
            JValue::Bool(b) => write!(f, "{}", b),
            JValue::String(s) => write!(f, "{}", s),
            JValue::Number(u) => write!(f, "{}", u),
            JValue::Array(a) => write!(f, "{}", a),
            JValue::Object(o) => write!(f, "{}", o),
        }
    }
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
            match tape.read() {
                Some('"') => { tape.step(1); break; },
                Some('\\') => {
                    tape.step(1);
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
                    } else {
                        return Err("bad escape".into());
                    }
                }
                Some(c) => { string.push(c); tape.step(1); }
                None => return Err("string not complete".into()),
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

    // Q: why operate on raw strings instead of regular strings?
    // A: so that escape sequences like linefeeds and unicodes are not decoded.
    let a = "\u{03A9}";
    let b = r#"\u{03A9}"#;
    println!("{:?}.len() = {}", a, a.len());
    println!("{:?}.len() = {}", b, b.len());

    /* json structure */
    let jstring: JValue = JString("value1".into()).into();
    let jnumber: JValue = JNumber::Int(42).into();
    let jbool:   JValue = JBool(true).into();
    let jnull:   JValue = JNull.into();
    println!("{}", jstring);
    println!("{}", jnumber);
    println!("{}", jbool);
    println!("{}", jnull);

    let mut arr: Vec<JValue> = Vec::new();
    arr.push(jbool);
    arr.push(jnull);
    let jarray: JValue = JArray(arr).into();
    println!("{}", jarray);

    let mut obj: HashMap<String, JValue> = HashMap::new();
    obj.insert("1st".into(), jstring);
    obj.insert("2nd".into(), jnumber);
    obj.insert("3rd".into(), jarray);
    let jobject: JValue = JObject(obj).into();
    println!("{}", jobject);

    /* json parser */
    let parser = Parser;

    // unit test
    let t1 = &mut Tape::new(r#"null"#);
    let t2 = &mut Tape::new(r#"true ,"#);
    let t3 = &mut Tape::new(r#"falsed"#);
    println!("{}", parser.read_null(t1).unwrap());
    println!("{}", parser.read_bool(t2).unwrap());
    println!("{}", parser.read_bool(t3).unwrap());

    let t4 = &mut Tape::new(r#"0"#);
    let t5 = &mut Tape::new(r#"5"#);
    let t6 = &mut Tape::new(r#"12.40"#);
    let t7 = &mut Tape::new(r#"-3.2e5 "#);
    println!("{}", parser.read_number(t4).unwrap());
    println!("{}", parser.read_number(t5).unwrap());
    println!("{}", parser.read_number(t6).unwrap());
    println!("{}", parser.read_number(t7).unwrap());

    let t8  = &mut Tape::new(r#""little""#);
    let t9  = &mut Tape::new(r#""big",  0.9"#);
    let t10 = &mut Tape::new(r#""\u03A9""#);
    println!("{}", parser.read_string(t8).unwrap());
    println!("{}", parser.read_string(t9).unwrap());
    println!("{}", parser.read_string(t10).unwrap());

    let t11 = &mut Tape::new(r#""backslash is \\""#);
    let t12 = &mut Tape::new("\"backslash is \\\\\"");
    println!("{}", parser.read_string(t11).unwrap());
    println!("{}", parser.read_string(t12).unwrap());

    let t13 = &mut Tape::new(r#"[null,  true,  [ "hello" ,   {"json": 1.2e-3}   ]  ] "#);
    let t14 = &mut Tape::new(r#"{"a":1, "bcd":  { "json": false, "xml": [null,2e-3]}}"#);
    println!("{}", parser.read_array(t13).unwrap());
    println!("{}", parser.read_object(t14).unwrap());

    // integration test
    let examples = vec![
        r#"  -3.2e5 "#,
        r#"["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]"#,
        r#"
        {
            "employee": {
                "name":       "\u03A9 Smith",
                "salary":     56000,
                "married":    true
                }
        }
        "#,
    ];
    examples.iter().for_each( |example|
        println!("{}", parser.read(example).unwrap())
    );

}
