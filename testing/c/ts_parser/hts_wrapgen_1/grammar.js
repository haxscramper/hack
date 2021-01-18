module.exports = grammar({
    name: "hts_wrapgen_1",
    externals: $ => [$.comment],
    extras: $ => [$.comment, /[\s\n]/],
    rules: {
        main: $ => repeat($.stmt),
        stmt: $ => 'hello'
    }
});
