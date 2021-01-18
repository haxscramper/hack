
module.exports = grammar({
    name: "simple_4",
    externals: $ => [$.comment],
    extras: $ => [$.comment, /\s\n/],
    rules: {
        main: $ => repeat($.stmt),
        stmt: $ => /[0-9]/
    }
});
