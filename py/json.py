# json parsing in python


NUMBERS = '0123456789'
_NUMBERS = '-0123456789'
CONSTANTS = {
    'null':  None,
    'true':  True,
    'false': False,
}
_CONSTANTS = ''.join(key[0:1] for key in CONSTANTS.keys())
ESCAPES = {
    '\\': '\\',
    '"':  '"',
    '/':  '/',
    'b':  '\b',
    'f':  '\f',
    'n':  '\n',
    'r':  '\r',
    't':  '\t',
}


class Tape:

    def __init__(self, data: str):
        self.data = data
        self.i = 0
        self.n = len(data)

    def look_ahead(self) -> str:
        if self.i+1 >= self.n:
            return ''
        return self.data[self.i+1]

    def look_behind(self) -> str:
        if self.i-1 < 0:
            return ''
        return self.data[self.i-1]

    def read(self) -> str:
        if self.i >= self.n:
            raise StopIteration
        return self.data[self.i]

    def step(self, k: int = 1):
        self.i += k

    def skip(self):
        while self.i < self.n and self.data[self.i].isspace():
            self.i += 1

    @property
    def rear(self) -> bool:
        return self.i >= self.n

    def __getitem__(self, key):
        return self.data.__getitem__(key)


class Parser:

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
        if c in '-123456789':
            pass
        elif c == '0':
            a = tape.look_ahead()
            assert not a or a not in NUMBERS
        else:
            raise ValueError('not a number')
        tape.step()
        while True:
            try:
                c = tape.read()
            except StopIteration:
                break
            if c in NUMBERS:
                pass
            elif c == '.':
                b = tape.look_behind()
                assert (
                    fraction is False and
                    exponent is False and
                    b and b in NUMBERS
                )
                fraction = True
            elif c in 'eE':
                b = tape.look_behind()
                assert (
                    exponent is False and
                    b and b in NUMBERS
                )
                exponent = True
            elif c in '+-':
                b = tape.look_behind()
                assert b and b in 'eE'
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
        assert tape.read() == '"', 'not a string'
        tape.step()
        try:
            while True:
                c = tape.read()
                tape.step()
                if c == '"':
                    break
                elif c == '\\':
                    d = tape.read()
                    if d in r'"\/bfnrt':
                        tape.step()
                        string.append(ESCAPES[d])
                    elif d == 'u':
                        tape.step()
                        try:
                            i = tape.i
                            string.append(chr(int(tape[i:i+4], 16)))
                        except ValueError as exc:
                            raise ValueError('bad hexadecimals') from exc
                        tape.step(4)
                    else:
                        raise ValueError('bad escape')
                else:
                    string.append(c)
        except StopIteration:
            raise ValueError('string not complete')
        return ''.join(string)

    def read_array(self, tape: Tape):
        array = list()
        assert tape.read() == '[', 'not an array'
        tape.step()
        try:
            tape.skip()
            c = tape.read()
            if c == ']':
                tape.step()
            else:
                while True:
                    tape.skip()
                    value = self.read_value(tape)
                    array.append(value)
                    tape.skip()
                    c = tape.read()
                    if c == ',':
                        tape.step()
                    elif c == ']':
                        tape.step()
                        break
                    else:
                        raise ValueError('array missing , or ]')
        except StopIteration:
            raise ValueError('array not complete')
        return array

    def read_object(self, tape: Tape):
        obj = dict()
        assert tape.read() == '{', 'not an object'
        tape.step()
        try:
            tape.skip()
            c = tape.read()
            if c == '}':
                tape.step()
            else:
                while True:
                    tape.skip()
                    key = self.read_string(tape)
                    tape.skip()
                    assert tape.read() == ':', 'object missing :'
                    tape.step()
                    tape.skip()
                    value = self.read_value(tape)
                    obj[key] = value
                    tape.skip()
                    c = tape.read()
                    if c == ',':
                        tape.step()
                    elif c == '}':
                        tape.step()
                        break
                    else:
                        raise ValueError('object missing , or }')
        except StopIteration:
            raise ValueError('object not complete')
        return obj

    def read_value(self, tape: Tape):
        c = tape.read()
        if c in _CONSTANTS:
            return self.read_constant(tape)
        elif c in _NUMBERS:
            return self.read_number(tape)
        elif c == '"':
            return self.read_string(tape)
        elif c == '[':
            return self.read_array(tape)
        elif c == '{':
            return self.read_object(tape)
        else:
            raise ValueError('bad value')

    def __call__(self, data: str):
        tape = Tape(data)
        tape.skip()
        value = self.read_value(tape)
        tape.skip()
        assert tape.rear, 'bad remainder'
        return value


if __name__ == '__main__':

    # Q: why operate on raw strings instead of regular strings?
    # A: so that escape sequences like linefeeds and unicodes are not decoded.
    a = u'\u03A9'
    b = r'\u03A9'
    print(f"len('{a}') = {len(a)}")
    print(f"len('{b}') = {len(b)}")

    # json parser
    parser = Parser()

    # unit test
    print(parser.read_number(Tape(r'0')))
    print(parser.read_number(Tape(r'5')))
    print(parser.read_number(Tape(r'12.40')))
    print(parser.read_number(Tape(r'-3.2e5 ')))
    print(parser.read_constant(Tape(r'falsed')))
    print(parser.read_constant(Tape(r'true ,')))
    print(parser.read_string(Tape(r'"little"')))
    print(parser.read_string(Tape(r'"big",  0.9')))
    print(parser.read_string(Tape(r'"\u03A9"')))
    print(parser.read_string(Tape(r'"backslash is \\"')))   # single escape
    print(parser.read_string(Tape('"backslash is \\\\"')))  # double escape
    print(parser.read_array(
        Tape(r'[null,  true,  [ "hello" ,   {"json": 1.2e-3}   ]  ] '))
    )
    print(parser.read_object(
        Tape(r'{"a": 1, "bcd":   {  "json": false, "xml": [null, 2e-3]}}'))
    )

    # integration test
    examples = [
        r'  -3.2e5 ',
        r'["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]',
        r"""
        {
            "employee": {
                "name":       "\u03A9 Smith",
                "salary":     56000,
                "married":    true
                }
        }
        """,
    ]
    for example in examples:
        print(parser(example))
