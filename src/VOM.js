"use strict";

exports["setAttribute'"] = function(name) {
  return function(listener) {
    return function(element) {
      return function() {
        element[name.toLowerCase()] = listener;
      };
    };
  };
};

exports["removeAttribute'"] = function(name) {
  return function(element) {
    return function() {
      element[name.toLowerCase()] = null;
    }
  }
}
