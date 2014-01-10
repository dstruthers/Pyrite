module Pyrite.VM where

import Control.Monad.Error (throwError)
import Pyrite.Types

initialVM = VM (Boolean False) [] []

eval :: VM -> Code -> ThrowsError VM
eval vm Exit = return vm
eval vm (Constant k next) = 
  eval vm { accumulator = k } next
