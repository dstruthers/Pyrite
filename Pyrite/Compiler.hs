module Pyrite.Compiler where

import Pyrite.Types

compile :: Value -> ThrowsError Code
compile (List (Symbol "quote":v:[])) = return $ Constant v Exit
compile (List sexpr) = Left $ CompileError "I don't know how to do application yet"
compile v = return $ Constant v Exit

