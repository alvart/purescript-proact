/*
  @license MIT
  Feed.js
*/

"use strict";

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

exports._feed = function () {
  return new Feed();
};

exports._notify = function (a, feed) {
  feed.notify(a)
};

exports._subscribe = function (observer, feed) {
  feed.subscribe(observer)
};
