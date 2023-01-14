grammar FSFallback;

main : text+ EOF;
text : structured_text | flat_text ;

structured_text : TEXT_START text_structure TEXT_END;
text_structure : bold | italic | WORD | SPACE ;
bold : BOLD_OPEN text_structure+ BOLD_CLOSE;
italic : ITALIC_OPEN text_structure+ ITALIC_CLOSE ;

flat_text : TEXT_START flat_element+ TEXT_END ;
flat_element : BOLD_OPEN | BOLD_CLOSE | ITALIC_OPEN | ITALIC_CLOSE | SPACE | WORD ;

BOLD_OPEN : '<b>' ;
BOLD_CLOSE : '</b>' ;
ITALIC_OPEN : '<i>' ;
ITALIC_CLOSE : '</i>' ;
TEXT_START : '<t>' ;
TEXT_END : '</t>' ;
SPACE : ' ' -> skip;
NEWLINE : '\n' -> skip;
WORD : [A-Z_]+ ;
