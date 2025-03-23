/* json parsing in javascript */


const SPACES = ' \n\r\t';
const DECIMALS = '0123456789';
const HEXADECIMALS = DECIMALS + 'abcdefABCDEF';
const CONSTANTS = {
    'null':  null,
    'true':  true,
    'false': false,
};
const ESCAPES = {
    [String.raw`\\`]: '\\',
    [String.raw`\"`]: '"',
    [String.raw`\/`]: '/',
    [String.raw`\b`]: '\b',
    [String.raw`\f`]: '\f',
    [String.raw`\n`]: '\n',
    [String.raw`\r`]: '\r',
    [String.raw`\t`]: '\t',
};


function isIndex(singleton) {
    if (
        singleton.length === 1         &&
        Number.isInteger(singleton[0])
    ) return true;
    return false;
}


function isSlice(pair) {
    if (
        pair.length === 2         &&
        Number.isInteger(pair[0]) &&
        Number.isInteger(pair[1])
    ) return true;
    return false;
}


class Tape {

    constructor(data) {
        this.i = 0;
        this.n = data.length;
        this.data = data;
    }

    slice(...args) {
        if (isIndex(args)) {
          return this.data[args[0]];
        }
        if (isSlice(args)) {
            return this.data.slice(args[0], args[1]);
        }
        return undefined;
      }

    lookAhead() {
        if (this.i+1 >= this.n) {
            return undefined;
        } else {
            return this.data[this.i+1];
        }
    }

    lookBehind() {
        if (this.i-1 < 0) {
            return undefined;
        } else {
            return this.data[this.i-1];
        }
    }

    read() {
        if (this.i >= this.n) {
            return undefined;
        } else {
            return this.data[this.i];
        }
    }

    step(k=1) {
        this.i += k;
    }

    skip() {
        while (this.i < this.n && SPACES.includes(this.data[this.i]))
            this.i += 1;
    }

    get rear() {
        return this.i >= this.n;
    }
}


class Parser {

    get constHeads () {
        return Object.keys(CONSTANTS)
            .map(key => key[0])
            .join('');
    }

    get numberHeads () {
        return '-0123456789';
    }

    readConstant (tape) {
        let i = tape.i;
        for (const [key, value] of Object.entries(CONSTANTS)) {
            let k = key.length;
            if (key == tape.slice(i,i+k)) {
                tape.step(k);
                return value;
            }
        }
        throw new Error('not a constant');
    }

    readNumber (tape) {
        let start = tape.i;
        let fraction = false;
        let exponent = false;
        let c = tape.read();
        if ('-123456789'.includes(c)) {
            // pass
        } else if (c === '0') {
            let a = tape.lookAhead();
            if (a !== undefined && DECIMALS.includes(a)) {
                throw new Error('invalid number');
            }
        } else {
            throw new Error('not a number');
        }
        tape.step();
        while (true) {
            c = tape.read();
            if (c === undefined) {
                break;
            }
            if (DECIMALS.includes(c)) {
                // pass
            } else if (c === '.') {
                if (
                    fraction ||
                    exponent ||
                    !DECIMALS.includes(tape.lookBehind())
                ) {
                    throw new Error('invalid decimal point');
                }
                fraction = true;
            } else if ('eE'.includes(c)) {
                if (exponent || !DECIMALS.includes(tape.lookBehind())) {
                    throw new Error('invalid exponent');
                }
                exponent = true;
            } else if ('+-'.includes(c)) {
                if (!'eE'.includes(tape.lookBehind())) {
                    throw new Error('sign must follow exponent');
                }
            } else {
                break;
            }
            tape.step();
        }
        const end = tape.i;
        const number = tape.slice(start, end);
        if (fraction || exponent) {
            return parseFloat(number);

        } else {
            return parseInt(number, 10);
        }
    }

    readString (tape) {
        let string = [];
        let c = tape.read();
        if (c !== '"') {
            throw new Error('not a string');
        }
        tape.step();
        while (true) {
            c = tape.read();
            tape.step();
            if (c === undefined) {
                throw new Error('string not complete');
            } else if (c === '"') {
                break;
            } else if (c === '\\') {
                if (String.raw`"\/bfnrt`.includes(tape.read())) {
                    tape.step();
                    string.push(ESCAPES[tape.slice(tape.i-2, tape.i)]);
                }
                else if (tape.read() === 'u') {
                    tape.step();
                    let u = parseInt(tape.slice(tape.i, tape.i+4), 16);
                    if (Number.isNaN(u)) {
                        throw new Error('bad hexadecimals');
                    } else {
                        string.push(String.fromCodePoint(u));
                    }
                    tape.step(4);
                } else {
                    throw new Error('bad escape');
                }
            } else {
                string.push(c);
            }
        }
        return string.join('');
    }

    readArray (tape) {
        let array = [];
        let value;
        let c = tape.read();
        if (c !== '[') {
            throw new Error('not an array');
        }
        tape.step();
        tape.skip();
        c = tape.read();
        if (c === undefined) {
            throw new Error('array not complete');
        }
        else if (c === ']') {
            tape.step();
        }
        else {
            while (true) {
                tape.skip();
                value = this.readValue(tape);
                array.push(value);
                tape.skip();
                c = tape.read();
                if (c === undefined) {
                    throw new Error('array not complete');
                } else if (c === ',') {
                    tape.step();
                } else if (c === ']') {
                    tape.step();
                    break;
                } else {
                    throw new Error('array missing , or ]');
                }
            }
        }
        return array;
    }

    readObject (tape) {
        let obj = {};
        let key, value;
        let c = tape.read();
        if (c !== '{') {
            throw new Error('not an object');
        }
        tape.step();
        tape.skip();
        c = tape.read();
        if (c === undefined) {
            throw new Error('object not complete');
        } else if (c === '}') {
            tape.step();
        } else {
            while (true) {
                tape.skip();
                key = this.readString(tape);
                tape.skip();
                c = tape.read();
                if (c === undefined) {
                    throw new Error('object not complete');
                } else if (c !== ':') {
                    throw new Error('object missing :');
                }
                tape.step();
                tape.skip();
                value = this.readValue(tape);
                obj[key] = value;
                tape.skip();
                c = tape.read();
                if (c === undefined) {
                    throw new Error('object not complete');
                } else if (c === ',') {
                    tape.step();
                } else if (c === '}') {
                    tape.step();
                    break;
                } else {
                    throw new Error('object missing , or }');
                }
            }
        }
        return obj;
    }

    readValue (tape) {
        let c = tape.read();
        if (this.constHeads.includes(c)) {
            return this.readConstant(tape);
        } else if (this.numberHeads.includes(c)) {
            return this.readNumber(tape);
        } else if (c === '"') {
            return this.readString(tape);
        } else if (c === '[') {
            return this.readArray(tape);
        } else if (c === '{') {
            return this.readObject(tape);
        } else {
            throw new Error('bad value');
        }
    }

    read (data) {
        let tape = new Tape(data);
        tape.skip();
        let value = this.readValue(tape);
        tape.skip();
        if (!tape.rear) {
            throw new Error('bad remainder');
        }
        return value;
    }
}


if (require.main === module) {

    // Q: why operate on raw strings instead of regular strings?
    // A: so that escape sequences like linefeeds and unicodes are not decoded.
    let a = '\u03A9';
    let b = String.raw`\u03A9`;
    console.log(`'${a}'.length = ${a.length}`);
    console.log(`'${b}'.length = ${b.length}`);

    // json parser
    parser = new Parser();

    // unit test
    console.log(parser.readConstant(new Tape(String.raw`falsed`)));
    console.log(parser.readConstant(new Tape(String.raw`true ,`)));
    console.log(parser.readNumber(new Tape(String.raw`0`)));
    console.log(parser.readNumber(new Tape(String.raw`5`)));
    console.log(parser.readNumber(new Tape(String.raw`12.40`)));
    console.log(parser.readNumber(new Tape(String.raw`-3.2e5 `)));
    console.log(parser.readString(new Tape(String.raw`"little"`)));
    console.log(parser.readString(new Tape(String.raw`"big",  0.9`)));
    console.log(parser.readString(new Tape(String.raw`"\u03A9"`)));
    console.log(parser.readString(new Tape(String.raw`"backslash is \\"`)));
    console.log(parser.readString(new Tape('"backslash is \\\\"')));
    console.log(parser.readArray(
        new Tape(String.raw`[null, true,  [ "hello" , {"json": 1.2e-3 } ]  ] `))
    );
    console.log(parser.readObject(
        new Tape(String.raw`{"a": 1, "bcd": {  "json": -2, "xml": [null, 3]}}`))
    );

    // integration test
    examples = [
        String.raw`  -3.2e5 `,
        String.raw`["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]`,
        String.raw`
        {
            "employee": {
                "name":       "\u03A9 Smith",
                "salary":     56000,
                "married":    true
                }
        }
        `,
    ];
    for (const example of examples) {
        console.log(parser.read(example));
    }

}
