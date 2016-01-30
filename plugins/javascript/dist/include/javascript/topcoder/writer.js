var tc = require("./errors");

function Writer() {
  this.text = "";
}

Writer.prototype.next = function(value, type) {
  if (typeof(value) === "undefined") {
    this.text += ", "
    return this;
  }

  if (type.substr(type.length - 2, 2) === "[]") {
    var elemType = type.substr(0, type.length - 2);
    this.text += '[';

    for (var i = 0; i < value.length; ++i) {
      this.next(value[i], elemType);
      if (i < value.length - 1) {
        this.next();
      }
    }

    this.text += ']';
    return this;
  }

  switch (type) {
  case "boolean":
  case "int":
  case "long":
  case "float":
  case "double":
    this.text += value.toString();
    return this;
  case "char":
    this.text += "'" + value + "'";
    return this;
  case "String":
    this.text += '"' + value + '"';
    return this;
  }
  throw new tc.UnsupportedType(type);
};

Writer.prototype.toString = function() {
  return this.text;
};

module.exports = Writer;
