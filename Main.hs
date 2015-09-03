{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad
import Text.Trifecta hiding (Parser)
import Text.Trifecta.Delta
import Text.PrettyPrint.ANSI.Leijen (putDoc)
import System.Environment
import System.Exit
import System.IO

import AST
import Parse
import PrettyPrint
import Eval

evaluate :: Decls String -> IO ()
evaluate decls = forever $ do
    putStr "> "
    input <- getLine
    when (null input) exitSuccess
    case parseString (runParser expr) (Lines 0 0 0 0) input of
        Failure errs -> putDoc errs
        Success e -> prettyPrint (nfWith decls e)
    putStrLn ""

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering

    (file:_) <- getArgs
    parseFromFile (runParser declarations) file >>= \case
        Nothing -> exitFailure
        Just d -> prettyPrint d >> evaluate d
