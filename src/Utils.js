// Global mutable state. Exposed to PureScript as Effect-typed FFI:
// every export returns a thunk (function () { ... }) so the PureScript
// type system can see the side effects.

var MAX_SPEED = 10.0;
var SPEED_STEP = 0.2;

var matrix = null;
var x      = 0.0;
var y      = 0.0;
var speed  = 0.0;

exports.storeX = function (val) {
  return function () {
    x = val;
  };
};

exports.storeY = function (val) {
  return function () {
    y = val;
  };
};

exports.getX = function () {
  return x;
};

exports.getY = function () {
  return y;
};

exports.storeMatrix = function (mat) {
  return function () {
    matrix = mat;
  };
};

exports.getCachedMatrix = function () {
  return matrix;
};

exports.incSpeed = function () {
  if (speed < MAX_SPEED) speed += SPEED_STEP;
  return speed;
};
