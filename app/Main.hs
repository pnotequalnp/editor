module Main where

import Data.Foldable (fold)
import Data.Version (showVersion)
import Editor.Client qualified as Client
import Editor.Client.Network.Channel qualified as Client.Channel
import Editor.Client.Network.Unix qualified as Client.Unix
import Editor.Network (getDefaultPath)
import Editor.Server qualified as Server
import Editor.Server.Network.Channel qualified as Server.Channel
import Editor.Server.Network.Unix qualified as Server.Unix
import Options.Applicative
import Paths_editor (version)
import UnliftIO.Async (async)
import UnliftIO.Chan (newChan)

data Mode = Client SocketInfo | Server SocketInfo | Standalone

data SocketType = Unix | TCP

data SocketInfo = SocketInfo
  { socketType :: SocketType,
    socketPath :: Maybe FilePath
  }

main :: IO ()
main = execParser (info parser cliOpts) >>= run
  where
    cliOpts = progDesc "Basic text editor. Almost certainly not useful."

run :: Mode -> IO ()
run Standalone = do
  toServer <- newChan
  toClient <- newChan
  env <- Server.initEnv
  _serverThread <- async . Server.server env $ Server.Channel.Channel toClient toServer
  Client.main $ Client.Channel.Channel toClient toServer
run (Client SocketInfo {socketPath}) = do
  path <- maybe getDefaultPath pure socketPath
  Client.Unix.init path >>= Client.main
run (Server SocketInfo {socketPath}) = do
  path <- maybe getDefaultPath pure socketPath
  Server.Unix.init path >>= Server.main

parser :: Parser Mode
parser = parseMode <**> helper <**> versioner

parseMode :: Parser Mode
parseMode =
  (serverFlag <|> clientFlag)
    <*> parseSocketInfo
    <|> pure Standalone

serverFlag :: Parser (SocketInfo -> Mode)
serverFlag = flag' Server (fold [short 'd', long "daemon", help "Run server daemon"])

clientFlag :: Parser (SocketInfo -> Mode)
clientFlag = flag' Client (fold [short 'c', long "client", help "Run client"])

parseSocketInfo :: Parser SocketInfo
parseSocketInfo = SocketInfo <$> parseSocketType <*> parsePath

parseSocketType :: Parser SocketType
parseSocketType = unixFlag <|> pure Unix

unixFlag :: Parser SocketType
unixFlag = flag' Unix $ fold [long "unix", help "Use Unix domain socket (default)"]

parsePath :: Parser (Maybe FilePath)
parsePath =
  optional . strOption $
    fold
      [ short 'p',
        long "path",
        help "Socket path",
        metavar "PATH"
      ]

versioner :: Parser (a -> a)
versioner =
  infoOption (showVersion version) $
    fold
      [ short 'v',
        long "version",
        help "Show version",
        hidden
      ]
