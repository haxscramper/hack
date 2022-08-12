%top{
  #include <iostream>
}

digit       [0-9]
number      {digit}+

%%

{number}    out() << "number " << text() << std::endl;
.
<<EOF>> out() << "finished input processing" << std::endl; return 0;

%%

int main() { return Lexer().lex(); }
