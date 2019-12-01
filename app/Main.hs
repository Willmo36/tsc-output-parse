module Main where

import Lib (fromTsc)
import qualified Config as C
import Options.Applicative (execParser, info, fullDesc)
import System.Directory

main :: IO ()
main = do
  (C.Config tscFile outDir) <- execParser $ info C.parseConfig fullDesc
  cwd <- getCurrentDirectory 
  -- readTsc tscFile outDir
  fromTsc cwd outDir
