grammar test_grammar;

main: expr EOF;
expr: expr ('*'|'/') expr
    | expr ('+'|'-') expr
    | INT
    | '(' expr ')'
    ;
INT     : [0-9]+ ;
NEWLINE : '\n' -> skip;
WHITESPACE : ' ' -> skip;
