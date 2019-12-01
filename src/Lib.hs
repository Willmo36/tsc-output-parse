{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.List (find)
import Data.Aeson (encodeFile)
import Data.Function (on)
import Data.Maybe
import Data.Tree
import System.IO
import TscParse
import qualified Text.Megaparsec as M
import Data.Aeson (encode)
import Process (runTscCommand)

fromTsc projDir outDir = do
    lines <-  runTscCommand projDir
    run lines outDir

fromFile tscFilePath outDir = do
    srcLines <- lines <$> readFile tscFilePath
    run srcLines outDir

run :: [String] -> [Char] -> IO ()
run srcLines outDir = do    
    let parsed = (M.parse tscLine "") <$> srcLines
    let parsedR = foldr go [] parsed
    let tree = removeLeafs $ labelWithCount $ foldr addPathToTree (Node "src" []) parsedR
    _ <- encodeFile (outDir ++ "/tree.json") tree
    _ <- writeFile (outDir  ++ "/tree.txt") $ drawTree (maybe (Node "failure" []) id tree)
    putStrLn "Complete"
        where
            go a as = either (const as) (\r -> r:as) a

addPathToTree :: [String] -> Tree String -> Tree String
addPathToTree [] tree = tree
addPathToTree [a] n@(Node ta ts) 
        | a == ta = Node ta ((Node "error" []):ts)
        | otherwise = n
addPathToTree path@(a:a2:as) tree@(Node ta ts)
        | ta == a   = Node ta nextTs
        | otherwise = tree -- different root dirs
    where
        as' = a2:as
        childMatch = find (\(Node ta2 _) -> ta2 == a2) ts  --if child match, recurse. Else, add as sibling
        nextTs = maybe ((treeFromArray as'):ts) (\_ -> (addPathToTree as') <$> ts) childMatch

treeFromArray :: [a] -> Tree a
treeFromArray [a] = Node a []
treeFromArray (a:as) =  Node a [treeFromArray as]

countLeafs :: Tree a -> Int
countLeafs (Node _ []) = 1
countLeafs (Node _ ts) = sum (countLeafs <$> ts)

labelWithCount :: Tree String -> Tree String
labelWithCount n@(Node l []) = n
labelWithCount (Node l ts) = let count = sum (countLeafs <$> ts) 
                             in Node (l ++ ": " ++ (show count)) (labelWithCount <$> ts)

--something is wrong with this
removeLeafs :: Tree a -> Maybe (Tree a)
removeLeafs (Node _ []) = Nothing
removeLeafs (Node l ts) = Just (Node l (foldr go [] ts))
    where
        go n acc =  maybe acc (\n2 -> n2:acc) (removeLeafs n)