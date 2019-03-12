var matrix = null;
var x = 0.0;
var y = 0.0;
var rotx = 0.0;
var roty = 0.0;

exports.storerotX = function(val) {
  rotx = val;
  console.log(val);
  return val;
}

exports.storerotY = function(val) {
  roty = val;
  console.log(val);
  return val;
}


exports.getrotX = function() {
  console.log(rotx);
  return rotx;
}

exports.getrotY = function() {
  console.log(roty);
  return roty;
}
exports.storeX = function(val) {
  x = val;
  console.log(val);
  return val;
}

exports.storeY = function(val) {
  y = val;
  console.log(val);
  return val;
}


exports.getX = function() {
  console.log(x);
  return x;
}

exports.getY = function() {
  console.log(y);
  return y;
}

exports.logAny = function(value) {
  console.log(value);
  return value;
}

exports.storeMatrix = function(mat) {
  matrix = mat;
  console.log("storeMatrix -> ", matrix);
  return matrix;
}

exports.getCachedMatrix = function() {
    console.log("matrix ->",matrix);
    return matrix;
}
