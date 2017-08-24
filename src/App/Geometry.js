"use strict"

exports.ptAngle = function(p1, p2) {
  var x1 = p1.value0
  var y1 = p1.value1
  var x2 = p2.value0
  var y2 = p2.value1

  return Math.atan2(y2 - y1, x2 - x1)
}

exports.distance = function(p1, p2) {
  var x1 = p1.value0
  var y1 = p1.value1
  var x2 = p2.value0
  var y2 = p2.value1

  return Math.sqrt(Math.pow(x2 - x1, 2) + Math.pow(y2 - y1, 2))
}

exports.strokeLength = function(stroke) {
  if (stroke.__proto__.constructor.name == "Line") {
    var x1 = stroke.value0.value0
    var y1 = stroke.value0.value1
    var x2 = stroke.value1.value0
    var y2 = stroke.value1.value1

    return Math.pow(x2-x1, 2) + Math.pow(y2-y1, 2)
  } else if (stroke.__proto__.constructor.name == "Arc") {
    var cx = stroke.value0.value0
    var cy = stroke.value0.value1
    var p1x = stroke.value1.value0
    var p1y = stroke.value1.value1
    var p2x = stroke.value2.value0
    var p2y = stroke.value2.value1
    var ccw = stroke.value3

    var angleDiff = Math.atan2(p2y - cy, p2x - cx) - Math.atan2(p1y - cy, p1x - cx)
    var sweep = angleDiff
    if (angleDiff < 0 && ccw) {
      sweep = angleDiff + 2.0 * Math.PI
    }

    if (angleDiff > 0 && !ccw) {
      sweep = angleDiff - 2.0 * Math.PI
    }

    if (angleDiff == 0) {
      if (ccw) {
        sweep = 2.0 * Math.PI
      } else {
        sweep = -2.0 * Math.PI
      }
    }

    return sweep * Math.sqrt(Math.pow(p1x - cx, 2) + Math.pow(p1y - cy, 2))
  } else {
    throw new Error("Unexpected stroke type in Geometry.strokeLength")
  }
}
