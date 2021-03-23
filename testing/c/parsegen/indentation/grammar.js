module.exports = grammar({
    name: "indentation",
    externals: $ => [$.newline, $.indent, $.dedent],
    extras: $ => [/ +/],
    rules: {
        main: $ => repeat($.stmt),
        simpleStmt: $ => choice(/[0-9]+/, /[a-z]+/),
        stmt: $ => choice($.simpleStmt, $.ifStmt),
        expr: $ => /[0-9]+/,
        block: $ => seq(repeat($.stmt), $.dedent),
        simpleStatements: $ => seq($.simpleStmt, $.newline),
        body: $ => choice(
            alias($.simpleStatements, $.block),
            seq($.indent, $.block),
            alias($.newline, $.block)
        ),

        ifStmt: $ => seq(
            $.ifBranch,
            repeat($.elifBranch),
            optional($.elseBranch),
        ),

        ifBranch: $ => seq('if', $.expr, ':', $.body),
        elifBranch: $ => seq('elif', $.expr, ':', $.body),
        elseBranch: $ => seq('else', ':', $.body),
    }
});
