-- code/Parser.hs

stmt :: Parser Stmt
stmt =
  try declAndAssignment
   <|> try times
   <|> try while
   <|> try varDecl
   <|> try arrAssignment
   <|> try assignment
   <|> try sIfThenElse
   <|> try sReturn
   <|> try sExpr
   <|> sIf
   <?> "Statement"

expr :: Parser Expr
expr = buildExpressionParser binops expr'
  where
    expr' :: Parser Expr
    expr' = try (parens expr <?> "parens")
         <|> try ePrim
         <|> try eFunCall
         <|> try eArrIdentifier
         <|> eIdentifier
         <?>  "factor"
