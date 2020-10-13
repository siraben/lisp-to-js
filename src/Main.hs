{-|
Module      : Main
Description : Main entry point for lisp-to-js.

-}
module Main where
import           SchemeRepl

main :: IO ()
main = do
  putStrLn
    "Welcome to the lisp-to-js, the Lisp to JS transpiler.\nInput an expression and press Enter to evaluate.\nPress Control-d to exit.\n"
  repl
