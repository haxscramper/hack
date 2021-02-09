module.exports = grammar({
    name: "statements",
    extras: $ => [/[\s\n]/],
    rules: {
        main: $ => repeat($.stmt),
        stmt: $ => choice($.commandExpr, $.callExpr, $.returnStmt),
        argList: $ => prec.left(seq($.expr, repeat(seq(',', $.expr)))),
        commandExpr: $ => prec.right(seq($.ident, optional($.argList))),
        callExpr: $ => seq($.ident, token.immediate('('), $.argList, ')'),
        returnStmt: $ => prec.right(seq('return', optional($.expr))),

        tuple: $ => seq('(', repeat(seq($.expr, optional(','))), ')'),
        expr: $ => prec(2, choice(
            $.callExpr,
            $.commandExpr,
            $.ident,
            $.tuple,
            seq('(', $.expr, ')')
        )),

        ident: $ => /\w+/,
    }
});
