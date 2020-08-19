/*
  @license MIT
  DOM.js
*/

"use strict";

var React = require("react");
var CamelCase = require("camelcase");

exports._createElement = function (class_) {
  return function (disp) {
    return function (props_) {
      var props = {};
      for (var key of Object.keys(props_)) {
        if (key === "style") {
          props.style = {};
          for (var css of props_.style) {
            props.style[CamelCase(css[0])] = css[1];
          }
        } else if (key.startsWith("on")) {
          props[key] = (function (key_) {
            return function () {
              disp(props_[key_].apply(props_[key_], arguments))();
            };
          })(key);
        } else {
          props[key] = props_[key];
        }
      }

      return function (children) {
        return React.createElement.apply(
          React,
          [class_, props].concat(children)
        );
      };
    };
  };
};

exports._fragment = React.Fragment;
