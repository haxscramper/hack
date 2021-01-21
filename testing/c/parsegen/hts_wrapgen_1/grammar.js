module.exports = grammar({
    name: "hts_wrapgen_1",
    externals: $ => [$.comment, $.space],
    extras: $ => [$.comment, $.space],
    rules: {
        source_file: $ => repeat($._definition),
        _definition: $ => choice($.function_definition),

        function_definition: $ => seq(
            'func', $.identifier, $.parameter_list, $._type, $.block),

        parameter_list: $ => seq('(', ')'),

        _type: $ => choice('bool'),

        block: $ => seq('{', repeat($._statement), '}'),

        _statement: $ => choice($.return_statement),

        return_statement: $ => seq('return', $._expression, ';'),
        _expression: $ => choice($.identifier, $.number),

        identifier: $ => /[a-z]+/,
        number: $ => /\d+/
    }
});
