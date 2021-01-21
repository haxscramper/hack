module.exports = grammar({
    name: "haxlink",
    conflicts: $ => [[$.namespace], [$.package]],
    rules: {
        main: $ => seq(
            field('package', optional($.package)),
            field('namespace', optional($.namespace)),
            field('entry', $.plainEntry),
            field('arguments', optional($.arglist)),
            field('element', optional($.elementPath))
        ),
        ident: $ => /[-_a-zA-Z0-9]+/,
        namespace: $ => repeat1(seq($.ident, '::')),
        package: $ => repeat1(seq($.ident, '/')),
        plainEntry: $ => prec.left(seq(
            $.ident, optional(seq('.', /[meitcp]/)))),
        arglist: $ => seq(
            '(',
            optional(seq($.type, repeat(seq(',', $.type)))),
            ')'
        ),
        type: $ => choice(
            $.ident,
            seq($.ident, '[', seq($.type, repeat(seq(',', $.type))), ']'),
            seq($.ident, '<', seq($.type, repeat(seq(',', $.type))), '>'),
        ),
        elementPath: $ => seq('.', $.ident)
    }
});
