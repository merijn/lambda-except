module Typecheck where

import Control.Monad.Trans.Free
import Data.Validation

import AST

data TypeError a = TypeError | UnknownVariables [a]

data VarType a = VarType (Expr a)

typeCheck :: Expr a -> Either (TypeError a) (Expr a)
typeCheck expression = case traverse changeVar expression of
    AccFailure errors -> Left (UnknownVariables errors)
    AccSuccess e -> typeOf e
  where
    changeVar :: a -> AccValidation [a] (VarType a)
    changeVar name = AccFailure [name]

typeOf :: Expr (VarType a) -> Either (TypeError a) (Expr a)
typeOf (Expr expression) = case runFree expression of
    Pure (VarType e) -> Right e
    Free e -> expr <$> case e of
        Const Star -> Right . Const $ Box
        Const Box -> Left TypeError
        Lam _ t body -> _

