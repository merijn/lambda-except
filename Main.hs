{-# LANGUAGE LambdaCase #-}
module Main where

import Bound
import Control.Monad
import Text.Trifecta hiding (Parser, err)
import Text.Trifecta.Delta
import Text.PrettyPrint.ANSI.Leijen (putDoc)
import System.Environment
import System.Exit
import System.IO

import AST
import Eval
import Parser
import PrettyPrint
import TypeCheck

evaluate :: Module String -> IO ()
evaluate m = do
    case closed m of
        Nothing -> putStrLn "Module has free variables!"
        Just tyModule -> do
            case typeCheckModule tyModule of
                Left e -> putStrLn e
                Right _ -> exprLoop tyModule
  where
    exprLoop :: Module Type -> IO ()
    exprLoop tyModule = forever $ do
        putStr "> "
        input <- getLine
        when (null input) exitSuccess
        case parseString (runParser expr) (Lines 0 0 0 0) input of
            Failure errs -> putDoc errs
            Success e -> evalExpr tyModule e
        putStrLn ""

    evalExpr :: Module Type -> Expr String -> IO ()
    evalExpr tyModule e = do
        let scopedExpr = abstractKeys (moduleDecls tyModule) e
        case typeCheckExpr tyModule <$> closed scopedExpr of
            Nothing -> putStrLn "Expression has free variables!"
            Just (Left err) -> putStrLn err
            Just (Right tyExpr) -> do
                prettyPrint tyExpr
                putStrLn ""
                prettyPrint (nfWith m e)

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering

    (file:_) <- getArgs
    parseFromFile (runParser moduleParser) file >>= \case
        Nothing -> exitFailure
        Just d -> prettyPrint d >> evaluate d
