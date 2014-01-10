module Main where

import Control.Monad (foldM, mapM)
import Pyrite.Compiler
import Pyrite.Parser
import Pyrite.Types
import Pyrite.VM
 
repl :: VM -> IO ()
repl vm = do
  putStr prompt
  src <- getLine
  if src == ""
    then repl vm
    else case parse src >>= mapM compile >>= foldM eval vm of
    Left e -> (putStrLn $ "***" ++ show e) >> repl vm
    Right vm' -> (putStrLn $ show vm') >> repl vm'
  where prompt = "Pyrite> "
