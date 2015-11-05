{-# LANGUAGE LambdaCase #-}
module Main where

import Bound
import Control.Monad
import Control.Monad.Trans
import Control.Lens
import Text.Trifecta (Result(..))
import Text.PrettyPrint.ANSI.Leijen (dullyellow, text)
import System.Console.Haskeline
import System.Environment
import System.Exit

import AST
import Eval
import Parser
import PrettyPrint
import TypeCheck

evaluate :: Module Name -> InputT IO ()
evaluate m = do
    case closed m of
        Nothing -> outputStrLn "Module has free variables!"
        Just tyModule -> do
            case typeCheckModule tyModule of
                Left e -> outputPretty e
                Right _ -> exprLoop tyModule
  where
    exprLoop :: Module (Named Type) -> InputT IO ()
    exprLoop tyModule = forever $ do
        outputStrLn ""
        input <- getInputLine "> "
        case input of
            Nothing -> liftIO exitSuccess
            Just s
                | null s -> liftIO exitSuccess
                | otherwise -> case parseFromData expr s of
                    Failure errs -> outputPretty errs
                    Success e -> evalExpr tyModule e

    evalExpr :: Module (Named Type) -> Expr Name -> InputT IO ()
    evalExpr tyModule e = do
        let scopedExpr = abstractKeys (tyModule^.decls) e
        case typeCheckExpr tyModule <$> closed scopedExpr of
            Nothing -> outputStrLn "Expression has free variables!"
            Just (Left err) -> outputPretty err
            Just (Right (Type tyExpr)) -> do
                outputPretty (nfWith m e)
                outputStr $ " " ++ show (dullyellow (text "::")) ++ " "
                outputPretty tyExpr

main :: IO ()
main = do
    (file:_) <- getArgs
    runInputT defaultSettings $ do
        parseFromFile moduleParser file >>= \case
            Failure errs -> outputPretty errs
            Success m -> outputPretty m >> evaluate m
