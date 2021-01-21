// Tree-sitter grammar to parse list of expressions and statements.
// Inherits expressions specification from `simple_1`

const simple_1 = require("../simple_1/grammar.js");

module.exports = grammar(simple_1, {
    name: "simple_2",
    rules: {
        main: $ => repeat($.stmt),
        stmt: $ => choice($.discardStmt, $.raiseStmt, $.expr),

        discardStmt: $ => prec.right(seq('discard', optional($.expr))),
        raiseStmt: $ => prec.right(seq('raise', optional($.expr))),
    }
});
