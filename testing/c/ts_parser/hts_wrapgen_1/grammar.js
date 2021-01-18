module.exports = grammar({
    name: "hts_wrapgen_1",
    externals: $ => [$.str],
    extras: $ => [$.str, /\s/],
    rules: {
        main: $ => repeat($.stmt),
        stmt: $ => /[0-9]/
    }
});
