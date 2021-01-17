// Reimplementation of lisp parser

module.exports = grammar({
    name: "emacs_rx",
    extras: $ => [$.comment, /[\s\n]/],
    rules: {
        main: $ => repeat($.form),
        literal: $ => /".*?"/,
        comment: $ => /;.*?\n/,
        ident: $ => /[-a-zA-Z:*\/]+/,

        any: $ => choice('any', 'in', 'char'),
        notAny: $ => seq('not', '(', $.any, repeat($.formElem), ')'),

        formHead: $ => choice(
            $.any,
            $.notAny
        ),

        lineStart: $ => choice('line-start', 'bol'),

        keyword: $ => choice(
            $.lineStart
        ),

        formElem: $ => choice(
            $.form,
            $.literal,
            $.keyword
        ),

        form: $ => seq('(', $.formHead, repeat($.formElem), ')')
    }
});
