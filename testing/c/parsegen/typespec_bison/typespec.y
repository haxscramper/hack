/* Type specification parser */

%{
  #include <stdio.h>
%}

%token IDENT

%%

type :
  IDENT[id] { printf("Regular ident %s\n", $id); }
| IDENT '[' typeList ']' { printf("Generic type %s\n", $1); }
;

typeList :
  typeList ',' type { printf("Multiple type arguments"); }
| type { printf("Type argument %s\n", $1); }
;

%%

int main(int argc, char **argv){
  printf("Parser main");
  yyparse();
  return 0;
}

int yyerror(char *s){
  fprintf(stderr, "error: %s\n", s);
  return 0;
}