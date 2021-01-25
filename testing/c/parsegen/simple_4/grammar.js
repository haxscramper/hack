module.exports = grammar({
    name: "simple_4",
    externals: $ => [$.COMMENT, $.SPACE],
    extras: $ => [$.COMMENT, $.WS, /\n/],
    rules: {
        main: $ => repeat($.stmt),
        WS: $ => alias($.SPACE, 'ws'),
        stmt: $ => /[0-9]/
    }
});
