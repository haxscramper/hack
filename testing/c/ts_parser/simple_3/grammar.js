// Grammar with 'extras' comments, based on simple_2

const simple_2 = require("../simple_2/grammar.js");

module.exports = grammar(simple_2, {
    name: "simple_3",
    extras: $ => [$.comment, /[\s\n]/],
    rules: { main: $ => repeat($.stmt), comment: $ => /{-.*?-}/ }
});
