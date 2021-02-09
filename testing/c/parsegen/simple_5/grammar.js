module.exports = grammar({
    name: "simple_5",
    externals: $ => [$.COMMENT],
    extras: $ => [$.COMMENT, /[\s\n]/],
    rules: {
        main: $ => repeat($.stmt),
        stmt: $ => /[0-9]/
    }
});
