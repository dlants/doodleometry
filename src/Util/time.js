"use strict";

exports.time = function () {
  return function (x) {
    return function (k) {
      console.time(x)
      var out = k({});
      console.timeEnd(x)
      return out
    };
  };
};
