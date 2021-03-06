{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : SchemeRepl
Description : REPL for lisp-to-js
-}
module SchemeRepl where

import           Translation
import           SchemeParser

import           System.Exit
import           System.IO
import qualified Data.Text.Lazy.IO             as TI
import qualified Data.Text.Lazy                as T
import           Language.JavaScript.Pretty.Printer
import           Language.JavaScript.Parser.AST
import           Language.JavaScript.Process.Minify

translate :: String -> Either T.Text JSAST
translate s = case readProg s of
  Right res -> convertP res
  Left  err -> Left "failed to parse"

-- Incidentally, calling minifyJS also fixes some problems with whitespace
report x = case x of
             Right res -> TI.putStrLn (renderToText (minifyJS res))
             Left  f   -> TI.putStrLn ("failed: " <> f)
-- |The main REPL loop.
repl :: IO ()
repl = do
  TI.putStr "lisp-to-js> "
  hFlush stdout
  done <- isEOF
  if done
    then TI.putStrLn "Exiting." *> exitSuccess
    else do
      exp <- getLine
      if exp == ""
        then repl
        else report (translate exp)
      repl

-- |Read, evaluate and print a file.
repf :: String -> IO ()
repf filename = do
  x <- openFile filename ReadMode
  y <- hGetContents x
  report (translate y)

-- |Read and print a file.
rpf :: String -> IO ()
rpf filename = do
  x <- openFile filename ReadMode
  y <- hGetContents x
  case readProg y of
    Right a -> print a
    Left a -> putStrLn $ "Failed to parse file: " ++ show a
