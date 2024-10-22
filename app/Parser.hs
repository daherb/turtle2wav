module Parser where

{-# LANGUAGE OverloadedStrings #-}

import Text.Parsec
import Text.Parsec.Char
import Data.ByteString
import Turtle

parseNumber :: Parsec ByteString u Int
parseNumber =
  read <$> many1 digit
  
parseOperation :: Parsec ByteString u Operation
parseOperation =
  choice [
    do
      choice [try (string s) | s <- ["fd", "forward"]]
      many1 space
      d <- parseNumber
      return $ Forward d
    ,
    do
      choice [ try (string s) | s<- ["backward", "back", "bk"]]
      many1 space
      d <- parseNumber
      return $ Backward d
    ,
    do
      choice [ try (string s) | s<- ["right", "rt"]]
      many1 space
      d <- parseNumber
      return $ Turtle.Right d
    ,
    do
      choice [ try (string s) | s<- ["left", "lt"]]
      many1 space
      d <- parseNumber
      return $ Turtle.Left d
    ,
    do
      choice [ try (string s) | s<- ["pendown", "down", "pd"]]
      many1 space
      return $ Pen Down
    ,
    do
      choice [ try (string s) | s<- ["penup", "up", "pu"]]
      many1 space
      return $ Pen Down
    ,
    do
      choice [ try (string s) | s<- ["pos", "position"]]
      return $ Position
    ,
      do
      try $ string "heading"
      return $ Heading
  ]
