# -*- coding: utf-8 -*-
# json parsing in python
# operate on bytes instead of strings (to avoid decoding and keep escapes)


SPACES = b' \n\r\t'
DECIMALS = b'0123456789'
HEXADECIMALS = DECIMALS + b'abcdefABCDEF'
CONSTANTS = {
    b'null':  None,
    b'true':  True,
    b'false': False,
}


class Tape:

    def __init__(self, data: bytes):
        self.data = data
        self.i = 0
        self.n = len(data)

    def look_ahead(self):
        if self.i+1 >= self.n:
            return None
        return self.data[self.i+1:self.i+2]

    def look_behind(self):
        if self.i-1 < 0:
            return None
        return self.data[self.i-1:self.i]

    def read(self):
        if self.i >= self.n:
            raise StopIteration
        return self.data[self.i:self.i+1]

    def step(self, k=1):
        self.i += k

    def skip(self):
        while self.i < self.n and self.data[self.i:self.i+1] in SPACES:
            self.i += 1

    @property
    def rear(self):
        return self.i >= self.n

    def __getitem__(self, key):
        return self.data.__getitem__(key)


class Parser:

    @property
    def const_heads(self):
        return b''.join(key[0:1] for key in CONSTANTS.keys())

    @property
    def number_heads(self):
        return b'-0123456789'

    def read_constant(self, tape: Tape):
        i = tape.i
        for const, value in CONSTANTS.items():
            k = len(const)
            if const == tape[i:i+k]:
                tape.step(k)
                return value
        raise ValueError('not a constant')

    def read_number(self, tape: Tape):
        start = tape.i
        fraction = False
        exponent = False
        c = tape.read()
        if c in b'-123456789':
            tape.step()
        elif c == b'0':
            tape.step()
            a = tape.read()
            assert a in b'.eE' or a in SPACES #or a is None
        else:
            raise ValueError('not a number')
        while True:
            try:
                c = tape.read()
            except StopIteration:
                break
            if c in DECIMALS:
                pass
            elif c == b'.':
                assert (
                    fraction is False and
                    exponent is False and
                    tape.look_behind() in DECIMALS
                )
                fraction = True
            elif c in b'eE':
                assert (
                    exponent is False and
                    tape.look_behind() in DECIMALS
                )
                exponent = True
            elif c in b'+-':
                assert tape.look_behind() in b'eE'
            else:
                break
            tape.step()
        end = tape.i
        number = tape[start:end]
        if fraction or exponent:
            return float(number)
        else:
            return int(number)

    def read_string(self, tape: Tape):
        string = list()
        assert tape.read() == b'"', 'not a string'
        tape.step()
        try:
            while True:
                c = tape.read()
                tape.step()
                if c == b'"':
                    break
                elif c == b'\\':
                    if tape.read() in b'"\\/bfnrt':
                        tape.step()
                        string.append(tape[tape.i-2:tape.i].decode('ascii'))
                    elif tape.read() == b'u':
                        tape.step()
                        try:
                            string.append(chr(int(tape[tape.i:tape.i+4], 16)))
                        except ValueError as exc:
                            raise ValueError('bad hexadecimals') from exc
                        tape.step(4)
                    else:
                        raise ValueError('bad escape')
                else:
                    string.append(c.decode('ascii'))
        except StopIteration:
            raise ValueError('string not complete')
        return ''.join(string)

    def read_array(self, tape: Tape):
        array = list()
        assert tape.read() == b'[', 'not an array'
        tape.step()
        try:
            tape.skip()
            c = tape.read()
            if c == b']':
                tape.step()
            else:
                while True:
                    tape.skip()
                    value = self.read_value(tape)
                    array.append(value)
                    tape.skip()
                    c = tape.read()
                    if c == b',':
                        tape.step()
                    elif c == b']':
                        tape.step()
                        break
                    else:
                        raise ValueError('array missing , or ]')
        except StopIteration:
            raise ValueError('array not complete')
        return array

    def read_object(self, tape: Tape):
        obj = dict()
        assert tape.read() == b'{', 'not an object'
        tape.step()
        try:
            tape.skip()
            c = tape.read()
            if c == b'}':
                tape.step()
            else:
                while True:
                    tape.skip()
                    key = self.read_string(tape)
                    tape.skip()
                    assert tape.read() == b':', 'object missing :'
                    tape.step()
                    tape.skip()
                    value = self.read_value(tape)
                    obj[key] = value
                    tape.skip()
                    c = tape.read()
                    if c == b',':
                        tape.step()
                    elif c == b'}':
                        tape.step()
                        break
                    else:
                        raise ValueError('object missing , or }')
        except StopIteration:
            raise ValueError('object not complete')
        return obj

    def read_value(self, tape: Tape):
        c = tape.read()
        if c in self.const_heads:
            return self.read_constant(tape)
        elif c in self.number_heads:
            return self.read_number(tape)
        elif c == b'"':
            return self.read_string(tape)
        elif c == b'[':
            return self.read_array(tape)
        elif c == b'{':
            return self.read_object(tape)
        else:
            raise ValueError('bad value')

    def __call__(self, data: bytes):
        tape = Tape(data)
        tape.skip()
        value = self.read_value(tape)
        tape.skip()
        assert tape.rear, 'bad remainder'
        return value


if __name__ == '__main__':

    parser = Parser()

    print(parser.read_number(Tape(b'12.40')))
    print(parser.read_number(Tape(b'-3.2e5 ')))
    print(parser.read_constant(Tape(b'falsed')))
    print(parser.read_constant(Tape(b'true ,')))
    print(parser.read_string(Tape(b'"little"')))
    print(parser.read_string(Tape(b'"big",  0.9')))
    print(parser.read_string(Tape(b'"\u03A9"')))
    print(parser.read_array(Tape(b'[null, true, ["hello", {"json":   1.2e-3}   ]]')))
    print(parser.read_object(Tape(b'{"a": 1, "bcd":   {  "json": false, "xml": [null, 2e-3]}}')))

    examples = [
        b'\r\t-3.2e5 \n',
        b'["Sunday",\t"Monday", "Tuesday", \n"Wednesday", \r "Thursday", "Friday",  "Saturday"]',
        b"""
        {
            "employee": {
                "name":       "\u03A9 Smith",
                "salary":      56000,
                "married":    true
                }
        }
        """,
    ]
    for example in examples:
        print(parser(example))
