// Tree-sitter grammar to parse list of simple arithmetic expressions

module.exports = grammar({
    name: "simple_1",
    rules: {
        main: $ => repeat($.expr),

        ident: $ => /[a-zA-Z]+/,
        literal: $ => /[0-9]+/,
        expr: $ => choice($.binaryExpr, $.ident, $.literal),
        binaryExpr: $ => choice(
            prec.left(2, seq($.expr, /[\\*]/, $.expr)),
            prec.left(2, seq($.expr, /[-+]/, $.expr)),
        )
    }
});
