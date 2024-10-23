module Parser where

{-# LANGUAGE OverloadedStrings #-}

import Text.Parsec
import Text.Parsec.Char
import Data.ByteString
import Turtle

parseNumber :: Parsec ByteString u Int
parseNumber =
  read <$> many1 digit
  
parseOperation :: Parsec ByteString u [Operation]
parseOperation =
  choice [
    do
      choice [try (string s) | s <- ["fd", "forward"]]
      many1 space
      d <- parseNumber
      return $ [Push d, Forward]
    ,
    do
      choice [ try (string s) | s<- ["backward", "back", "bk"]]
      many1 space
      d <- parseNumber
      return $ [Push d, Backward]
    ,
    do
      choice [ try (string s) | s<- ["right", "rt"]]
      many1 space
      d <- parseNumber
      return $ [Push d, Turtle.Right]
    ,
    do
      choice [ try (string s) | s<- ["left", "lt"]]
      many1 space
      d <- parseNumber
      return $ [Push d, Turtle.Left]
    ,
    do
      choice [ try (string s) | s<- ["pendown", "down", "pd"]]
      many1 space
      return $ [Push 1, Pen]
    ,
    do
      choice [ try (string s) | s<- ["penup", "up", "pu"]]
      many1 space
      return $ [Push 0, Pen]
    ,
    do
      choice [ try (string s) | s<- ["pos", "position"]]
      return $ [Position]
    ,
    do
      choice [ try (string s) | s<- ["head", "heading"]]
      return $ [Heading]
   ,
   do
      try $ string "home"
      return $ Turtle.home
  ]
