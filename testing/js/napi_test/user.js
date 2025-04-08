// test.js
const addon = require('./build/myaddon.node');

const obj = new addon.Wrapper(42);
console.log(obj.getValue());
console.log(obj.toString());

const shared = addon.createShared(99);
console.log(shared.getValue());
console.log(shared.toString());
