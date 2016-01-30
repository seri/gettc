exports.UnsupportedType = function(type) {
  this.type = type;

  this.toString = function() {
    return "UnsupportedType: " + type.toString() + " is not a valid TopCoder type";
  };
}
