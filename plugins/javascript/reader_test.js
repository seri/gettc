var Reader = require("./dist/include/javascript/topcoder/reader");

exports.testUnsupportedType = function(test) {
    var reader = new Reader("whatever");
    test.throws(function() {
        reader.next("object");
    });
    test.done();
};


exports.testBooleanTrue = function(test) { try {
    var reader = new Reader("  tRuE ");
    test.strictEqual(reader.next("boolean"), true);
    test.done();
} catch (err) {
    console.log(err.toString());
    console.trace();
}};

exports.testBooleanFalse = function(test) { try {
    var reader = new Reader("fALsE");
    test.strictEqual(reader.next("boolean"), false);
    test.done();
} catch (err) {
    console.log(err.toString());
    console.trace();
}};

exports.testBooleanError = function(test) {
    var reader = new Reader("whatever");
    test.throws(function() {
        reader.next("boolean");
    });
    test.done();
};


exports.testPositiveInt = function(test) { try {
    var reader = new Reader("20.14");
    test.strictEqual(reader.next("int"), 20);
    test.done();
} catch (err) {
    console.log(err.toString());
    console.trace();
}};

exports.testNegativeInt = function(test) { try {
    var reader = new Reader("-5210");
    test.strictEqual(reader.next("int"), -5210);
    test.done();
} catch (err) {
    console.log(err.toString());
    console.trace();
}};

exports.testIntError = function(test) {
    var reader = new Reader("NaN");
    test.throws(function() {
        reader.next("int");
    });
    test.done();
};

exports.testLongInt = function(test) { try {
    var str = "1234567890123456789";
    var reader = new Reader(str);
    test.strictEqual(reader.next("long").toString(), str);
    test.done();
} catch (err) {
    console.log(err.toString());
    console.trace();
}};


exports.testPositiveFloat = function(test) { try {
    var reader = new Reader("20.14.123");
    test.strictEqual(reader.next("float"), 20.14);
    test.done();
} catch (err) {
    console.log(err.toString());
    console.trace();
}};

exports.testNegativeFloat = function(test) { try {
    var reader = new Reader("-6210");
    test.strictEqual(reader.next("float"), -6210);
    test.done();
} catch (err) {
    console.log(err.toString());
    console.trace();
}};



exports.testQuotedChar = function(test) { try {
    var reader = new Reader("'@'");
    test.strictEqual(reader.next("char"), '@');
    test.done();
} catch (err) {
    console.log(err.toString());
    console.trace();
}};

exports.testUnquotedChar = function(test) { try {
    var reader = new Reader("@");
    test.strictEqual(reader.next("char"), '@');
    test.done();
} catch (err) {
    console.log(err.toString());
    console.trace();
}};

exports.testCharError = function(test) {
    var reader = new Reader("'@");
    test.throws(function() {
        reader.next("char");
    });
    test.done();
};


exports.testString = function(test) { try {
    var reader = new Reader("\t\"Tyrion <> Lannister\"");
    test.strictEqual(reader.next("String"), "Tyrion <> Lannister");
    test.done();
} catch (err) {
    console.log(err.toString());
    console.trace();
}};

exports.testStringWithQuote = function(test) { try {
    var reader = new Reader("\"The \"Little Finger\"\", how are you?");
    test.strictEqual(reader.next("String"), "The \"Little Finger\"");
    test.done();
} catch (err) {
    console.log(err.toString());
    console.trace();
}};

exports.testStringError = function(test) {
    var reader = new Reader("I am not quoted properly");
    test.throws(function() {
        reader.next("String");
    });
    test.done();
};


exports.testEmptyArray = function(test) { try {
    var reader = new Reader("[]");
    test.deepEqual(reader.next("int[]"), []);
    test.done();
} catch (err) {
    console.log(err.toString());
    console.trace();
}};

exports.testArray1D = function(test) { try {
    var reader = new Reader(" [ true, false, false, false, true ] ");
    test.deepEqual(reader.next("boolean[]"), 
                  [true, false, false, false, true]);
    test.done();
} catch (err) {
    console.log(err.toString());
    console.trace();
}};

exports.testArray2D = function(test) { try {
    var reader = new Reader("[[\"abc\", \"def\"], []]");
    test.deepEqual(reader.next("String[][]"), [["abc", "def"], []]);
    test.done();
} catch (err) {
    console.log(err.toString());
    console.trace();
}};

exports.testArrayError = function(test) {
    var reader = new Reader("[1, 2, 3");
    test.throws(function() {
        reader.next("int[]");
    });
    test.done();
};


exports.testEverything = function(test) { try {
    var text = "[[\"Jon Snow\", \"Lord Varys\", \"The \"Little Finger\"\"], [ ]]\n";
    text += ", C, 20.14, fAlSe, 'x', [ -2 , 0 , 1 , 4 ]";
    var reader = new Reader(text);
    test.deepEqual(reader.next("String[][]"), [
        ["Jon Snow", "Lord Varys", "The \"Little Finger\""], []
    ]);
    reader.next();
    test.strictEqual(reader.next("char"), 'C');
    reader.next();
    test.strictEqual(reader.next("double"), 20.14);
    reader.next();
    test.strictEqual(reader.next("boolean"), false);
    reader.next();
    test.strictEqual(reader.next("char"), 'x');
    reader.next();
    test.deepEqual(reader.next("int[]"), [-2, 0, 1, 4]);
    test.done();
} catch (err) {
    console.log(err.toString());
    console.trace();
}};
