module.exports = grammar({
    name: "indentation",
    externals: $ => [$.COMMENT],
    extras: $ => [$.COMMENT, /[\s\n]/],
    rules: {
        main: $ => repeat($.stmt),
        stmt: $ => /[0-9]/
    }
});
