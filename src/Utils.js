var matrix = null;
var x      = 0.0;
var y      = 0.0;
var rotx   = 0.0;
var roty   = 0.0;
var speed  = 0.0;

exports.storerotX = function(val) {
  rotx = val;
  //return function(){
    return val;
  //}
}

exports.storerotY = function(val) {
  roty = val;
  //return function(){
    return val;
  //}
}


exports.getrotX = function() {
    return rotx;
}

exports.getrotY = function() {
    return roty;
}
exports.storeX = function(val) {
  x = val;
  //return function(){
    return val;
  //}
}

exports.storeY = function(val) {
  y = val;
  //return function(){
    return val;
  //}
}


exports.getX = function() {
    return x;
}

exports.getY = function() {
    return y;
}

exports.logAny = function(value) {
  console.log(value);
  //return function(){
    return value;
  //}
}

exports.storeMatrix = function(mat) {
  matrix = mat;
  //console.log("storeMatrix -> ", matrix);
  //return function(){
    return matrix;
  //}
}

exports.getCachedMatrix = function() {
    console.log(speed);
    return matrix;
}

exports.getSpeed = function() {
  return speed;
}

exports.incSpeed = function() {
  if (speed < 10.0)
    speed += 0.2;
  return speed;
}

exports.decSpeed = function() {
  if (speed > -10)
    speed -= 0.2
  return speed;
}

exports.off = function() {
  speed = 0.0;
  return;
}
