module Main where

import Control.Category ((>>>))
import Data.Char (toLower)
import Data.Foldable (fold)
import Data.Version (showVersion)
import Editor.Client qualified as Client
import Editor.Client.Network.Unix qualified as Client.Unix
import Editor.Network (getDefaultPath)
import Editor.Server qualified as Server
import Editor.Server.Network.Unix qualified as Server.Unix
import Options.Applicative
import Paths_editor (version)

data Mode = Client | Server

data Network = Unix | TCP

data Opts = Opts
  { mode :: Mode,
    path :: Maybe String,
    network :: Maybe Network
  }

main :: IO ()
main = execParser (info parser cliOpts) >>= execute
  where
    cliOpts =
      fold
        [ progDesc "Basic text editor. Almost certainly not useful."
        ]

execute :: Opts -> IO ()
execute Opts {mode, path} = do
  addr <- maybe Editor.Network.getDefaultPath pure path
  case mode of
    Client -> Client.Unix.init addr >>= Client.main
    Server -> Server.Unix.init addr >>= Server.main

parser :: Parser Opts
parser = opts <**> helper <**> versioner

opts :: Parser Opts
opts = Opts <$> parseMode <*> parsePath <*> parseNetwork

versioner :: Parser (a -> a)
versioner =
  infoOption (showVersion version) $
    fold
      [ short 'v',
        long "version",
        help "Show version",
        hidden
      ]

parseMode :: Parser Mode
parseMode =
  flag Client Server $
    fold
      [ short 'd',
        long "daemon",
        help "Run server daemon"
      ]

parseNetwork :: Parser (Maybe Network)
parseNetwork =
  optional . option (maybeReader p) $
    fold
      [ short 'b',
        long "backend",
        help "Socket type (currently ignored)",
        metavar "UNIX|TCP"
      ]
  where
    p =
      fmap toLower >>> \case
        "unix" -> Just Unix
        "tcp" -> Just TCP
        _ -> Nothing

parsePath :: Parser (Maybe String)
parsePath =
  optional . strOption $
    fold
      [ short 'p',
        long "path",
        help "Socket path",
        metavar "PATH"
      ]
