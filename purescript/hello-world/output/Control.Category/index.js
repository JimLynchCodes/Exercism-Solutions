// Generated by purs version 0.13.5
"use strict";
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Category = function (Semigroupoid0, identity) {
    this.Semigroupoid0 = Semigroupoid0;
    this.identity = identity;
};
var identity = function (dict) {
    return dict.identity;
};
var categoryFn = new Category(function () {
    return Control_Semigroupoid.semigroupoidFn;
}, function (x) {
    return x;
});
module.exports = {
    Category: Category,
    identity: identity,
    categoryFn: categoryFn
};
