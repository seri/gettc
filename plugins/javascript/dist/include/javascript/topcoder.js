var errors = require("./topcoder/errors");

module.exports = {
  Reader : require("./topcoder/reader"),
  Writer : require("./topcoder/writer"),
  UnsupportedType : errors.UnsupportedType
};
