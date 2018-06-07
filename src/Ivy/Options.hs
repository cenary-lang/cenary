module Ivy.Options where

------------------------------------------------------
import           Control.Applicative
import           Data.Monoid ((<>))
import           Data.String (IsString, fromString)
import           Options.Applicative
------------------------------------------------------
------------------------------------------------------

data Options = Options
  { _mode      :: Mode
  , _inputFile :: FilePath
  }

data Mode = Ast | ByteCode | Run | Disasm | Asm | Deploy | RewindDeploy
  deriving Show

instance IsString Mode where
  fromString "ast"           = Ast
  fromString "bytecode"      = ByteCode
  fromString "run"           = Run
  fromString "disasm"        = Disasm
  fromString "asm"           = Asm
  fromString "deploy"        = Deploy
  fromString "rewind-deploy" = RewindDeploy
  fromString _               = Run

parseMode :: Parser Mode
parseMode =
  fromString <$> strOption (long "mode" <> short 'm' <> help "Mode of the execution. Available options: ast, bytecode, run, disasm, asm")

optParser :: Parser Options
optParser = Options
        <$> parseMode
        <*> strOption (long "input" <> short 'i' <> help "Input file that contains ivy code" )

parseOptions :: IO Options
parseOptions =
  execParser (info (optParser <**> helper) (fullDesc <> progDesc "Ivy" ))
