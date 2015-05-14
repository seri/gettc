var tc = require("./errors");

function ParseError(text, pos, info) {
    if (pos < text.length && pos >= 0) {
        text = text.substr(0, pos) + "|" + 
               text.substr(pos, 1) + "|" +
               text.substr(pos + 1);
    }
    this.message = "<" + text + ">";
    if (typeof(info) === "string") {
        this.message += " (" + info + ")";
    }

    this.toString = function() {
        return "ParseError: " + this.message;
    };
}


function Reader(text) {
    this.text = text;
    this.pos = 0;
    this.len = text.length;
}

Reader.prototype.next = function(type) {
    if (typeof(type) === "undefined") {
        spaces.call(this);
        expect.call(this, ',');
        return ;
    }
    if (type.substr(type.length - 2, 2) === "[]") {
        return nextArray.call(this, type.substr(0, type.length - 2));
    }
    switch (type) {
        case "boolean":
            return nextBoolean.call(this);
        case "int":
            return nextInt32.call(this);
        case "long":
            return nextInt64.call(this);
        case "float":
        case "double":
            return nextFloat.call(this);
        case "char":
            return nextChar.call(this);
        case "String":
            return nextString.call(this);
        default:
            throw new tc.UnsupportedType(type);
    }
};


function isWhiteSpace() {
    var c = this.text.charAt(this.pos);
    return c === ' ' || c === '\t' || c === '\n';
}

function isDigit(c) {
    var c = this.text.charAt(this.pos);
    return c >= '0' && c <= '9';
}


function raiseHere(message) {
    throw new ParseError(this.text, this.pos, message);
}

function checkPos() {
    if (this.pos >= this.len) {
        raiseHere.call(this, "unexpected end of input");
    }
}


function token() {
    checkPos.call(this);
    return this.text.charAt(this.pos);
}

function spaces() {
    while (this.pos < this.len && isWhiteSpace.call(this)) {
        this.pos += 1;
    }
}

function expect(character) {
    if (token.call(this) === character) {
        this.pos += 1;
    } else {
        raiseHere(this, "expecting <" + character + ">");
    }
}


function nextBoolean() {
    spaces.call(this);
    if (this.text.substr(this.pos, 4).toUpperCase() === "TRUE") {
        this.pos += 4;
        return true;
    } else if (this.text.substr(this.pos, 5).toUpperCase() === "FALSE") {
        this.pos += 5;
        return false;
    }
    raiseHere.call(this, "expecting either true or false");
}


function nextDigits() {
    checkPos.call(this);
    if (!isDigit.call(this)) {
        raiseHere.call(this, "expecting a digit");
    }
    var start = this.pos;
    while (true) {
        this.pos += 1;
        if (this.pos === this.len || !isDigit.call(this)) {
            break;
        }
    }
    return this.text.substr(start, this.pos - start);
}


function nextNaturalNumber(parser) {
    spaces.call(this);
    if (token.call(this) === "-") {
        this.pos += 1;
        return -parser(nextDigits.call(this));
    }
    return parser(nextDigits.call(this));
}

function nextInt32() {
    return nextNaturalNumber.call(this, function (str) {
        return parseInt(str, 10);
    });
}

function nextInt64() {
    var Int64 = require("long");
    return nextNaturalNumber.call(this, function (str) {
        return Int64.fromString(str);
    });
}


function nextPositiveFloat() {
    var str = nextDigits.call(this);
    if (this.pos < this.len) {
        if (this.text.charAt(this.pos) === '.') {
            this.pos += 1;
            str += "." + nextDigits.call(this);
        }
    }
    return parseFloat(str);
}

function nextFloat() {
    spaces.call(this);
    if (token.call(this) === '-') {
        this.pos += 1;
        return -nextPositiveFloat.call(this);
    }
    return nextPositiveFloat.call(this);
}


function nextChar() {
    spaces.call(this);
    var c = token.call(this);
    if (c === '\'') {
        this.pos += 1;
        var ret = token.call(this);
        this.pos += 1;
        expect.call(this, '\'');
        return ret;
    }
    this.pos += 1;
    return c;
}


function nextString() {
    spaces.call(this);
    expect.call(this, '"');
    var start = this.pos;
    while (true) {
        if (this.pos >= this.len) {
            raiseHere.call(this, "expecting a closing quote");
        }
        if (token.call(this) === '"') {
            this.pos += 1;
            var saved = this.pos;
            spaces.call(this);
            if (this.pos === this.len || 
                this.text.charAt(this.pos) === ',' ||
                this.text.charAt(this.pos) === ']') {
                this.pos = saved;                
                return this.text.substr(start, this.pos - 1 - start);
            }
        } else {
            this.pos += 1;
        }
    }
}


function nextElems(elemType, arr) {
    spaces.call(this);
    var c = token.call(this);
    if (c === ']') {
        this.pos += 1;
        return ;
    } else if (c === ',') {
        this.pos += 1;
        arr.push(this.next(elemType));
        nextElems.call(this, elemType, arr);
        return ;
    }
    raiseHere("expecting either <,> or <]>");
}

function nextArray(elemType) {
    var arr = [];
    spaces.call(this);
    expect.call(this, '[');
    spaces.call(this);
    if (token.call(this) === ']') {
        this.pos += 1;
        return arr;
    }
    arr.push(this.next(elemType));
    nextElems.call(this, elemType, arr);
    return arr;
}

module.exports = Reader;
