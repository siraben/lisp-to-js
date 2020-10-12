{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : SchemeRepl
Description : REPL for LispToJS

-}
module SchemeRepl where

import Translation
import SchemeParser
import SchemeTypes
import System.Exit
import System.IO
import qualified Data.Text.Lazy.IO as TI
import qualified Data.Text.Lazy as T
import Language.JavaScript.Pretty.Printer
import Language.JavaScript.Process.Minify

reval s =
  case readProg s of
    Right res -> convertP res
    Left err  -> Left "failed to parse"

-- |The main REPL loop.
repl :: IO ()
repl = do
  TI.putStr "lisp-to-js> "
  hFlush stdout
  done <- isEOF
  if done
    then TI.putStrLn "Exiting." *> exitSuccess
    else do
      exp <- TI.getLine
      if exp == ""
        then repl
        else case reval (T.unpack exp) of
               Right res -> TI.putStrLn (renderToText (minifyJS res))
               Left f -> TI.putStrLn ("failed: " <> f)
      repl

-- -- |Read, evaluate and print a file.
-- repf :: String -> IO ()
-- repf filename = do
--   x <- openFile filename ReadMode
--   y <- hGetContents x
--   reportResult $ reval y

-- -- |Read and print a file.
-- rpf :: String -> IO ()
-- rpf filename = do
--   x <- openFile filename ReadMode
--   y <- hGetContents x
--   case readProg y of
--     Right a -> print a
--     Left a -> putStrLn $ "Failed to parse file: " ++ show a
