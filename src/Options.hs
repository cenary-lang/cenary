module Options where

------------------------------------------------------
import           Control.Applicative
import           Data.Monoid         ((<>))
import           Data.String         (IsString, fromString)
import           Options.Applicative
------------------------------------------------------
------------------------------------------------------

data Options = Options
  { _mode      :: Mode
  , _inputFile :: FilePath
  }

data Mode = Debug | Run | Disasm

instance IsString Mode where
  fromString "debug"  = Debug
  fromString "run"    = Run
  fromString "disasm" = Disasm

debug :: Mode -> Bool
debug Debug = True
debug _     = False

optParser :: Parser Options
optParser = Options
        <$> strOption ( long "mode" <> short 'm' <> help "Mode of the execution. `debug` or `run`" )
        <*> strOption (long "input" <> short 'i' <> help "Input file that contains ivy code" )

parseOptions :: IO Options
parseOptions =
  execParser (info (optParser <**> helper) (fullDesc <> progDesc "Ivy" ))
