{-# LANGUAGE OverloadedStrings #-}

module Config where

import Options.Applicative
import Data.Semigroup ((<>))

data Config = Config {tscFilePath :: String, outPath :: String }

tscFilePathParse = strOption (long "tscFilePath" <> short 'i')
outPathParse = strOption (long "outDir" <> short 'o')

parseConfig :: Parser Config
parseConfig = Config <$> tscFilePathParse <*> outPathParse