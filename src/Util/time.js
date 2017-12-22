"use strict";

exports.timeAny = function () {
  return function (x) {
    return function (k) {
      console.time(x)
      k({});
      return console.timeEnd(x)
    };
  };
};
