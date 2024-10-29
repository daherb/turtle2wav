module Turtle (Operation(..), home) where

import Control.Monad.Trans.State.Lazy
import Data.List

data Operation = Forward | Backward | Left | Right | Pen | Heading | Position | Push Int | Pop | Dup | Swap | Add | Sub | Label Int | Jumpz Int deriving Show


home :: [Operation]
home =
  [
    Heading
  , Dup
  , Dup
  , Sub
  , Sub
  , Turtle.Left
  ]
type Canvas = [[Int]]

run :: [Operation] -> Int -> Int -> [Canvas]
run ops =
  let (result, state) = runState (evalOps ops) []
  in
    nub result
  where
    evalOps :: [Operation] -> State [Int] [Canvas]
    evalOps [] = return []
