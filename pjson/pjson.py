# -*- coding: utf-8 -*-
# json parsing in python


SPACES = ' \n\r\t'
DECIMALS = '0123456789'
HEXADECIMALS = DECIMALS + 'abcdefABCDEF'
CONSTANTS = {
    'null':  None,
    'true':  True,
    'false': False,
}


class Tape:

    def __init__(self, text):
        self.text = text
        self.i = 0
        self.n = len(text)

    def look_ahead(self):
        if self.i+1 >= self.n:
            return None
        return self.text[self.i+1]

    def look_behind(self):
        if self.i-1 < 0:
            return None
        return self.text[self.i-1]

    def read(self):
        if self.i >= self.n:
            raise StopIteration
        return self.text[self.i]

    def step(self, k=1):
        self.i += k

    def skip(self):
        while self.i < self.n and self.text[self.i] in SPACES:
            self.i += 1

    @property
    def rear(self):
        return self.i >= self.n

    def __getitem__(self, key):
        return self.text.__getitem__(key)


class Parser:

    @property
    def const_inits(self):
        return ''.join(key[0] for key in CONSTANTS.keys())

    @property
    def num_inits(self):
        return '-0123456789'

    def read_constant(self, tape):
        i = tape.i
        for const, value in CONSTANTS.items():
            k = len(const)
            if const == tape[i:i+k]:
                tape.step(k)
                return value
        raise ValueError('not a constant')

    def read_number(self, tape):
        start = tape.i
        fraction = False
        exponent = False
        c = tape.read()
        if c in '-123456789':
            tape.step()
        elif c == '0':
            tape.step()
            a = tape.read()
            assert a in '.eE' or a in SPACES or a is None
        else:
            raise ValueError('not a number')
        while True:
            try:
                c = tape.read()
            except StopIteration:
                break
            if c in DECIMALS:
                pass
            elif c == '.':
                assert (
                    fraction is False and
                    exponent is False and
                    tape.look_behind() in DECIMALS
                )
                fraction = True
            elif c in 'eE':
                assert (
                    exponent is False and
                    tape.look_behind() in DECIMALS
                )
                exponent = True
            elif c in '+-':
                assert tape.look_behind() in 'eE'
            else:
                break
            tape.step()
        end = tape.i
        number = tape[start:end]
        if fraction or exponent:
            return float(number)
        else:
            return int(number)

    def read_string(self, tape):
        start = tape.i
        assert tape.read() == '"', 'not a string'
        tape.step()
        try:
            while True:
                c = tape.read()
                tape.step()
                if c == '\\':
                    if tape.read() in '"\\/bfnrt':
                        tape.step()
                    elif tape.read() == 'u':
                        tape.step()
                        try:
                            int(tape[tape.i:tape.i+4], 16)
                        except ValueError as exc:
                            raise ValueError('bad hexadecimals') from exc
                        tap.step(4)
                    else:
                        raise ValueError('bad escape')
                elif c == '"':
                    break
        except StopIteration:
            raise ValueError('string not complete')
        end = tape.i
        return tape[start+1:end-1]

    def read_array(self, tape):
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

    def read_object(self, tape):
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

    def read_value(self, tape):
        c = tape.read()
        if c in self.const_inits:
            return self.read_constant(tape)
        elif c in self.num_inits:
            return self.read_number(tape)
        elif c == '"':
            return self.read_string(tape)
        elif c == '[':
            return self.read_array(tape)
        elif c == '{':
            return self.read_object(tape)
        else:
            raise ValueError('bad value')

    def __call__(self, text):
        tape = Tape(text)
        tape.skip()
        value = self.read_value(tape)
        tape.skip()
        assert tape.rear, 'bad remainder'
        return value


if __name__ == '__main__':

    parser = Parser()

    print(parser.read_number(Tape('12.40')))
    print(parser.read_number(Tape('-3.2e5 ')))
    print(parser.read_constant(Tape('falsed')))
    print(parser.read_constant(Tape('true ,')))
    print(parser.read_string(Tape('"little"')))
    print(parser.read_string(Tape('"big",  0.9')))
    print(parser.read_array(Tape('["\u0391", true, ["hello", {"json":   1.2e-3}   ]]')))
    print(parser.read_object(Tape('{"a": 1, "bcd":   {  "json": true, "xml": [null, 2e-3]}}')))

    examples = [
        '-3.2e5 \n',
        '["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"]',
        """
        {
            "employee": {
                "name":       "sonoo",
                "salary":      56000,
                "married":    true
                }
        }
        """,
    ]
    for example in examples:
        print(parser(example))
