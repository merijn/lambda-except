{-# LANGUAGE LambdaCase #-}
module Main where

import Bound
import Control.Lens
import Control.Monad
import Text.Trifecta (Result(..))
import Text.PrettyPrint.ANSI.Leijen (putDoc, dullyellow, text)
import System.Environment
import System.Exit
import System.IO

import AST
import Eval
import Parser
import PrettyPrint
import TypeCheck

evaluate :: Module Name -> IO ()
evaluate m = do
    case closed m of
        Nothing -> putStrLn "Module has free variables!"
        Just tyModule -> do
            case typeCheckModule tyModule of
                Left e -> putDoc e >> putStrLn ""
                Right _ -> exprLoop tyModule
  where
    exprLoop :: Module (Named Type) -> IO ()
    exprLoop tyModule = forever $ do
        putStr "> "
        input <- getLine
        when (null input) exitSuccess
        case parseFromData expr input of
            Failure errs -> putDoc errs >> putStrLn ""
            Success e -> evalExpr tyModule e
        putStrLn ""

    evalExpr :: Module (Named Type) -> Expr Name -> IO ()
    evalExpr tyModule e = do
        let scopedExpr = abstractKeys (tyModule^.decls) e
        case typeCheckExpr tyModule <$> closed scopedExpr of
            Nothing -> putStrLn "Expression has free variables!"
            Just (Left err) -> putDoc err >> putStrLn ""
            Just (Right tyExpr) -> do
                prettyPrint (nfWith m e)
                putStr $ " " ++ show (dullyellow (text "::")) ++ " "
                prettyPrint tyExpr

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering

    (file:_) <- getArgs
    parseFromFile moduleParser file >>= \case
        Failure errs -> putDoc errs >> putStrLn "" >> exitFailure
        Success m -> prettyPrint m >> putStrLn "" >> evaluate m
