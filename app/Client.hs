module Main where

import Editor.Client qualified as Client
import Editor.Client.Network.Unix qualified as Unix

main :: IO ()
main = Unix.init "/tmp/editor" >>= Client.main
