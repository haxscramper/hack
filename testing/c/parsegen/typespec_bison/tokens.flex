%{
  /* #define YYSTYPE char * */
  /* extern YYSTYPE yylval; */
  enum yytokentype {
    IDENT = 258,
  };
%}

%%

[a-zA-Z][a-zA-Z0-9]* { return IDENT; }
