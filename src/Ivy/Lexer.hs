{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Ivy.Lexer where

--------------------------------------------------------------------------------
import qualified Text.Parsec.Token    as Tok
--------------------------------------------------------------------------------
import           Ivy.Lexer.Language        (style)
--------------------------------------------------------------------------------

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style

integer       = Tok.integer lexer
float         = Tok.float lexer
parens        = Tok.parens lexer
commaSep      = Tok.commaSep lexer
semiSep       = Tok.semiSep lexer
identifier    = Tok.identifier lexer
whitespace    = Tok.whiteSpace lexer
reserved      = Tok.reserved lexer
reservedOp    = Tok.reservedOp lexer
natural       = Tok.natural lexer
operator      = Tok.operator lexer
symbol        = Tok.symbol lexer
charLiteral   = Tok.charLiteral lexer
stringLiteral = Tok.stringLiteral lexer
braces = Tok.braces lexer
