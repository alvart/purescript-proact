/*
  @license MIT
  DOM.js
*/

"use strict";

var React = require("react");

var Feed = function () {
  this.subscribers = [];

  this.subscribe = function (observer) {
    this.subscribers.push(observer);
  }.bind(this);

  this.notify = function (a) {
    for (var observer of this.subscribers) {
      observer(a)();
    }
  }.bind(this);
};

exports._createElement = function (class_) {
  return function (props_) {
    return function (children) {
      var props = {};
      var feedList = [];
      for (var key of Object.keys(props_)) {
        if (key.startsWith("_on")) {
          var feed = new Feed();

          props[key.substr(1)] = function (key_) {
            return function () {
              feed.notify(props_[key_].apply(null, arguments));
            };
          }(key);

          feedList.push(feed);
        } else {
          props[key] = props_[key];
        }
      }

      return {
        element: React.createElement.apply(
          React,
          [class_, props].concat(children)
        ),
        feedList,
      };
    };
  };
};

exports._fragment = React.Fragment;
