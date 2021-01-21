// define a grammar called Hello
grammar typespec;
main   : ID
       | ID '[' ID ( ','  ID )* ']'
       ;

ID     : [a-z]+
       ;

WS     : [ \t\r\n]+ -> skip
       ;
