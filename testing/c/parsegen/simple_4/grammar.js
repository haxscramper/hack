module.exports = grammar({
    name: "simple_4",
    externals: $ => [$.COMMENT],
    extras: $ => [$.COMMENT, /[\s\n]/],
    rules: {
        main: $ => repeat($.stmt),
        stmt: $ => /[0-9]/
    }
});
