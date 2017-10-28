-- code/Parser.hs

factor :: Parser Expr
factor = try (parens expr <?> "parens")
     <|> try timesIterationBegin
     <|> try prims
     <|> try varDecl
     <|> try arrAssignment
     <|> try assignment
     <|> try identifier'
     <|> debug
     <?> "factor"
