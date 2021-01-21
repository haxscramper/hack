module.exports = grammar({
    name: "typespec",
    rules: {
        main: $ => $.type,
        ident: $ => /[a-zA-Z]+/,
        type: $ => choice(
            $.ident,
            seq($.ident,
                '[', optional(seq($.type, repeat(seq(',', $.type)))), ']')
        )
    }
});
