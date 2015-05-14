var Writer = require("./dist/include/javascript/topcoder/writer");

exports.testWriteAll = function(test) { try {
    var writer = new Writer();
    writer.next("Seri", "String");
    writer.next();
    writer.next('M', "char");
    writer.next();
    writer.next(false, "boolean");
    writer.next();
    writer.next(20.14, "double");
    writer.next();
    writer.next([1, 2, 3, 4], "int[]");
    var result = "\"Seri\", 'M', false, 20.14, [1, 2, 3, 4]";
    test.strictEqual(writer.toString(), result);
    test.done();
} catch (err) {
    console.log(err.toString());
    console.trace();
}};

exports.testModule = function(test) { try {
    var tc = require("./dist/include/javascript/topcoder");
    var reader = new tc.Reader("abc");
    var writer = new tc.Writer();
    var err = new tc.UnsupportedType();
    test.done();
} catch (err) {
    console.log(err.toString());
    console.trace();
}};
