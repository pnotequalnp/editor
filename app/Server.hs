module Main where

import Editor.Server qualified as Server
import Editor.Server.Network.Unix qualified as Unix

main :: IO ()
main = Unix.init "/tmp/editor" >>= Server.main
