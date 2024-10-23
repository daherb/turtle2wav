module Turtle (Operation(..), home) where

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
