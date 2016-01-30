#! /usr/bin/env node

var fs = require("fs");
var path = require("path");
var tc = null;

function init() {
  var gettcHome = process.env.GETTC_HOME || path.join(process.env.HOME, ".gettc");
  var includeDir = path.join(gettcHome, "include", "javascript");
  tc = require(path.join(includeDir, "topcoder"));
}

function main() {
  try {
    var input = fs.readFileSync(process.argv[2], { encoding: "ascii" });
    var reader = new tc.Reader(input);
        var X = reader.next('int[]'); reader.next();
    var Y = reader.next('int[]'); reader.next();
    var R = reader.next('int[]'); reader.next();
    var x1 = reader.next('int'); reader.next();
    var y1 = reader.next('int'); reader.next();
    var x2 = reader.next('int'); reader.next();
    var y2 = reader.next('int');

    var CirclesCountry = require("./CirclesCountry");
    var result = CirclesCountry.leastBorders(X, Y, R, x1, y1, x2, y2);

    var writer = new tc.Writer();
    writer.next(result, "int");
    fs.writeFileSync(process.argv[3], writer.toString(), { encoding: "ascii" });
  } catch (err) {
    console.log(err.toString());
    console.trace();
  }
}

init();
main();
