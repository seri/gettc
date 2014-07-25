from topcoder.errors import UnsupportedTypeError

class ReadError(Exception):
    def __init__(self, text, pos, info = ""):
        if (pos < len(text)) and (pos >= 0):
            text = text[:pos] + "|" + text[pos] + "|" + text[(pos + 1):]
        message = "<" + text + ">"
        if info:
            message += " (" + info + ")"
        Exception.__init__(self, message)

class Reader(object):
    def __init__(self, text):
        self._internal = ReaderInternal(text)

    def next(self, type = None):
        return self._internal.next(type)

class ReaderInternal(object):
    def __init__(self, text):
        self.text = text
        self.pos = 0
        self.len = len(text)

    def next(self, type = None):
        if type is None:
            self.spaces()
            self.expect(',')
            return

        if type == "int" or type == "long":
            return self.next_int()
        if type == "float" or type == "double":
            return self.next_float()
        if type == "char":
            return self.next_char()
        if type == "String":
            return self.next_string()
        if type == "boolean":
            return self.next_boolean()
        if type.endswith("[]"):
            return self.next_array(type[:-2])
        
        raise UnsupportedTypeError(type)

    def raise_here(self, message):
        raise ReadError(self.text, self.pos, message)

    def check_pos(self):
        if self.pos >= self.len:
            self.raise_here("unexpected end of input")

    def token(self):
        self.check_pos()
        return self.text[self.pos]

    def spaces(self):
        while self.pos < self.len and self.token().isspace():
            self.pos += 1

    def next_digits(self):
        if not self.token().isdigit():
            self.raise_here("expecting a digit")
        begin = self.pos
        while True:
            self.pos += 1
            if self.pos == self.len or not self.token().isdigit():
                break
        return self.text[begin:self.pos]

    def next_positive_int(self):
        return int(self.next_digits())
            
    def next_int(self):
        self.spaces()
        if self.token() == "-":
            self.pos += 1
            return -self.next_positive_int()
        return self.next_positive_int()

    def next_positive_float(self):
        s = self.next_digits()
        if self.pos < self.len:
            if self.token() == ".":
                self.pos += 1
                s += "." + self.next_digits()
        return float(s)

    def next_float(self):
        self.spaces()
        if self.token() == "-":
            self.pos += 1
            return -self.next_positive_float()
        return self.next_positive_float()

    def expect(self, c):
        if self.token() == c:
            self.pos += 1
        else:
            self.raise_here("expecting <" + c + ">")

    def next_char(self):
        self.spaces()
        self.expect("'")
        c = self.token()
        self.pos += 1
        self.expect("'")
        return c

    def next_boolean(self):
        self.spaces()
        if self.text[self.pos:self.pos + 4].upper() == "TRUE":
            self.pos += 4
            return True
        elif self.text[self.pos:self.pos + 5].upper() == "FALSE":
            self.pos += 5
            return False
        self.raise_here("expecting either true or false)")

    def next_string(self):
        self.spaces()
        self.expect('"')
        begin = self.pos
        while True:
            if self.pos >= self.len:
                self.raise_here("expecting a closing quote when reading a string")
            if self.token() == '"':
                self.pos += 1
                saved = self.pos
                self.spaces()
                if self.pos == self.len or self.token() == ',' or self.token() == ']': 
                    self.pos = saved
                    return self.text[begin:self.pos - 1]
            else:
                self.pos += 1

    def next_elems(self, elem_type, array):
        self.spaces()
        c = self.token()
        if c == "]":
            self.pos += 1
            return
        elif c == ",":
            self.pos += 1
            array.append(self.next(elem_type))
            self.next_elems(elem_type, array)
        else:
            self.raise_here("expecting either <,> or <]>")

    def next_array(self, elem_type):
        result = []
        self.spaces()
        self.expect("[")
        self.spaces()
        if self.token() == "]":
            self.pos += 1
            return result
        result.append(self.next(elem_type))
        self.next_elems(elem_type, result)
        return result