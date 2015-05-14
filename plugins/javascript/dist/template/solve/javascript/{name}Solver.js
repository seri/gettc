#! /usr/bin/env node
<% engine = JavascriptEngine.new func, vars %>
var fs = require("fs");
var path = require("path");
var tc = null;

function init() {
    var gettcHome = process.env.GETTC_HOME || path.join(process.env.HOME, ".gettc");
    var includeDir = path.join(gettcHome, "include", "javascript");
    tc = require(path.join(includeDir, "topcoder"));
}

function main() { try {
    var input = fs.readFileSync(process.argv[2], { encoding: "ascii" });
    var reader = new tc.Reader(input);
<%= engine.input.gsub(/^/, " " * 4) %>

    var <%= prob.name %> = require("./<%= prob.name %>");
    var result = <%= prob.name %>.<%= func.name %>(<%= engine.arglist %>);
    
    var writer = new tc.Writer();
    writer.next(result, "<%= func.type.to_s %>");
    fs.writeFileSync(process.argv[3], writer.toString(), { encoding: "ascii" });
} catch (err) {
    console.log(err.toString());
    console.trace();
}}

init();
main();
