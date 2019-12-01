{-# LANGUAGE OverloadedStrings          #-}

module TscParse where

import Data.Void
import Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Text.Megaparsec.Error as M

type TscParser = Parsec Void String

dir :: TscParser String
dir = do
  M.satisfy (== '/')
  M.some $ M.noneOf ['/', '(']

lineColumn :: TscParser (String, String)
lineColumn = do
  M.char '('
  line <- M.some $ M.numberChar
  M.char(',')
  col <- M.some $ M.numberChar
  M.char(')')
  return (line, col)

tscLine :: TscParser [String]
tscLine = do
  root <- M.string "src"
  dirs <- M.some dir
  (line, col) <- lineColumn
  M.takeRest 
  return (root:dirs ++ [formatLineCol line col])

formatLineCol l c = "(" ++ l ++ ", " ++ c ++ ")"