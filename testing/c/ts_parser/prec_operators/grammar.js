// More complicated operator parsing rules, with different levels of
// precedence

module.exports = grammar({
    name: "prec_operators",
    rules: {
        main: $ => repeat($.expr),
        expr: $ => choice($.literal, $.ident, $.binaryExpr, $.unaryExpr),

        ident: $ => /[a-zA-Z]+/,
        literal: $ => /[0-9]+/,

        OP3: $ => /[+\-~|][+\-*\/<@$~&%|!?^.:\\]*/,
        OP2: $ => /[@:?][=+\-*\/<>@$~&%|!?^.:\\]*/,
        OP1: $ => /[=+\-*\/<>@$~&%|!?^.:\\]+=/,
        OP0: $ => /[-=]+[>]+/,

        unaryExpr: $ => choice(
            prec.left(4, seq($.OP3, $.expr)),
            prec.left(3, seq($.OP2, $.expr)),
            prec.left(2, seq($.OP1, $.expr)),
            prec.left(1, seq($.OP0, $.expr)),
        ),

        binaryExpr: $ => choice(
            prec.left(4, seq($.expr, $.OP3, $.expr)),
            prec.left(3, seq($.expr, $.OP2, $.expr)),
            prec.left(2, seq($.expr, $.OP1, $.expr)),
            prec.left(1, seq($.expr, $.OP0, $.expr)),
        ),
    }
});
