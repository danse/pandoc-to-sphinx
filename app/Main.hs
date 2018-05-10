{-# LANGUAGE PatternGuards, OverloadedStrings #-}
module Main where

import Text.Pandoc
import Text.Pandoc.JSON
import Text.Pandoc.Error (handleError)
import Text.Pandoc.Walk (walk, query)
import Text.Pandoc.Options (ReaderOptions(..))
import System.Environment (getArgs)
import Data.Text (Text(..), unlines, pack)
import qualified Data.Text.IO as T
import qualified Data.Map as M
import Data.Monoid ((<>))
import Control.Applicative ((<$>))
import Data.Maybe (maybeToList)
import Data.Either (fromRight)
import Options.Applicative
import Control.Monad (when, unless)


data Options = Options {
  dir :: Str,
  levelOption :: Maybe Int
  }

toSphinx :: Options -> IO ()
toSphinx (Options dir level) = T.getContents >>=
                handleError . readJSON def >>=
                transform dir level >>
                pure ()

options :: Parser Options
options = Options
          <$> argument str (metavar "OUTPUT_DIR" <>
                            help "the directory where to save the Sphinx files")
          <*> option auto (long "level" <>
                           short 'l' <>
                           help "the section level to use for splitting the document" <>
                           showDefault)

main = execParser (info options fullDesc) >>= toSphinx
