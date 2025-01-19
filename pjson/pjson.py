# -*- coding: utf-8 -*-
# json parsing in python

from enum import Flag, auto


class ArrayState(Flag):
    COMMA = auto()
    CLOSE = auto()
    VALUE = auto()


class ObjectState(Flag):
    COLON = auto()
    COMMA = auto()
    CLOSE = auto()
    VALUE = auto()
    KEY   = auto()


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

    def __getitem__(self, key):
        return self.text.__getitem__(key)


class Parser:

    SPACES = ' \n\r\t'
    DIGITS = '0123456789'
    CONSTANTS = {
        'null':  None,
        'true':  True,
        'false': False,
    }

    @property
    def const_inits(self):
        return ''.join(key[0] for key in self.CONSTANTS.keys())

    @property
    def num_inits(self):
        return '-0123456789'

    def read_constant(self, tape):
        i = tape.i
        for const, value in self.CONSTANTS.items():
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
            pass
        elif c == '0':
            a = tape.look_ahead()
            assert a in '.eE' or a in self.SPACES or a is None
        else:
            raise ValueError('not a number')
        tape.step()
        while True:
            try:
                c = tape.read()
            except StopIteration:
                break
            if c in self.DIGITS:
                pass
            elif c == '.':
                assert (
                    fraction is False and
                    exponent is False and
                    tape.look_behind() in self.DIGITS
                )
                fraction = True
            elif c in 'eE':
                assert (
                    exponent is False and
                    tape.look_behind() in self.DIGITS
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
        c = tape.read()
        if c == '"':
            tape.step()
        else:
            raise ValueError('not a string')
        while True:
            try:
                c = tape.read()
            except StopIteration:
                raise ValueError('string not enclosed')
            tape.step()
            if c == '\\':
                if tape.read() in '"\\/bfnrt':
                    tape.step()
                elif tape.read() == 'u':
                    tape.step()
                    try:
                        int(tape[tape.i:tape.i+4])
                    except ValueError as exc:
                        raise ValueError('bad hex digits') from exc
                    tap.step(4)
                else:
                    raise ValueError('bad escape')
            elif c == '"':
                break
        end=tape.i
        return tape[start+1:end-1]

    def read_array(self, tape):
        array = list()
        c = tape.read()
        if c == '[':
            state = ArrayState.CLOSE | ArrayState.VALUE
            tape.step()
        else:
            raise ValueError('not an array')
        while True:
            try:
                c = tape.read()
            except StopIteration:
                raise ValueError('array not enclosed')
            if c in self.const_inits:
                assert ArrayState.VALUE in state
                array.append(self.read_constant(tape))
                state = ArrayState.COMMA | ArrayState.CLOSE
            elif c in self.num_inits:
                assert ArrayState.VALUE in state
                array.append(self.read_number(tape))
                state = ArrayState.COMMA | ArrayState.CLOSE
            elif c == '"':
                assert ArrayState.VALUE in state
                array.append(self.read_string(tape))
                state = ArrayState.COMMA | ArrayState.CLOSE
            elif c == '[':
                assert ArrayState.VALUE in state
                array.append(self.read_array(tape))
                state = ArrayState.COMMA | ArrayState.CLOSE
            elif c == '{':
                assert ArrayState.VALUE in state
                array.append(self.read_object(tape))
                state = ArrayState.COMMA | ArrayState.CLOSE
            elif c in self.SPACES:
                tape.step()
            elif c == ',':
                assert ArrayState.COMMA in state
                state = ArrayState.VALUE
                tape.step()
            elif c == ']':
                assert ArrayState.CLOSE in state
                tape.step()
                break
            else:
                raise ValueError('bad array')
        return array

    def read_object(self, tape):
        obj = dict()
        key, value = None, None
        state = ObjectState.CLOSE | ObjectState.KEY
        c = tape.read()
        if c == '{':
            tape.step()
        else:
            raise ValueError('not an object')
        while True:
            try:
                c = tape.read()
            except StopIteration:
                raise ValueError('object not enclosed')
            if c in self.const_inits:
                assert ObjectState.VALUE in state
                value = self.read_constant(tape)
                obj[key] = value
                state = ObjectState.COMMA | ObjectState.CLOSE
            elif c in self.num_inits:
                assert ObjectState.VALUE in state
                value = self.read_number(tape)
                obj[key] = value
                state = ObjectState.COMMA | ObjectState.CLOSE
            elif c == '"':
                if ObjectState.VALUE in state:
                    value = self.read_string(tape)
                    obj[key] = value
                    state = ObjectState.COMMA | ObjectState.CLOSE
                elif ObjectState.KEY in state:
                    key = self.read_string(tape)
                    state = ObjectState.COLON
                else:
                    assert ObjectState.VALUE in state or ObjectState.KEY in state
            elif c == '[':
                assert ObjectState.VALUE in state
                value = self.read_array(tape)
                obj[key] = value
                state = ObjectState.COMMA | ObjectState.CLOSE
            elif c == '{':
                assert ObjectState.VALUE in state
                value = self.read_object(tape)
                obj[key] = value
                state = ObjectState.COMMA | ObjectState.CLOSE
            elif c in self.SPACES:
                tape.step()
            elif c == ',':
                assert ObjectState.COMMA in state
                state = ObjectState.KEY
                tape.step()
            elif c == ':':
                assert ObjectState.COLON in state
                state = ObjectState.VALUE
                tape.step()
            elif c == '}':
                assert ObjectState.CLOSE in state
                tape.step()
                break
            else:
                raise ValueError('bad object')
        return obj

    def __call__(self, text):
        tape = Tape(text.strip(self.SPACES))
        c = tape.read()
        if c in self.const_inits:
            value = self.read_constant(tape)
        elif c in self.num_inits:
            value = self.read_number(tape)
        elif c == '"':
            value = self.read_string(tape)
        elif c == '[':
            value = self.read_array(tape)
        elif c == '{':
            value = self.read_object(tape)
        else:
            raise ValueError('not a json')
        assert tape[tape.i:].strip() == ''
        return value


if __name__ == '__main__':

    parser = Parser()

    print(parser.read_number(Tape('12.40')))
    print(parser.read_number(Tape('-3.2e5 ')))
    print(parser.read_constant(Tape('falsed')))
    print(parser.read_constant(Tape('true ,')))
    print(parser.read_string(Tape('"little"')))
    print(parser.read_string(Tape('"big",  0.9')))
    print(parser.read_array(Tape('[0.9, true, ["hello", {"json":   1.2e-3}   ]]')))
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

    # ToDo: custom exception, unit test
