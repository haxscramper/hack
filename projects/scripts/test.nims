import pegs

let pegAst = """
sexp <- list / integer / string / ident
list <- '(' sexp+ ')'

ident <- ws (':' / \w / '-')+ ws

string <- ws '"' {_} '"'
integer <- ws \d+ ws
ws <- (\s)*
""".peg
