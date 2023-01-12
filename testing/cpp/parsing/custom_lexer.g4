grammar custom_lexer;

main : orgBold Space? EOF ;

orgBold : BoldOpen Ident BoldClose ;

BoldOpen : 'BoldOpen';
BoldClose : 'BoldClose';
Ident : 'Ident';
Space : 'Space';


