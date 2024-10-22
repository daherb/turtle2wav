module Turtle where

data PenPosition = Up | Down deriving Show
data Operation = Forward Int | Backward Int | Left Int | Right Int | Pen PenPosition | Heading | Position deriving Show
