{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
module AST
    ( Loc(..)
    , Type(..)
    , Decl(..)
    , Decls
    , Expr(..)
    , buildDecls
    , buildLet
    ) where

import Prelude.Extras

import Bound
import Control.Monad
import Data.Bifunctor
import Data.List
import Text.Trifecta.Delta (Delta)

data Loc = Undefined | Loc Delta Delta
    deriving (Eq,Ord,Show)

data Type
    = TyBool Loc
    | TyInt Loc
    | TyArr Type Type Loc
    deriving (Eq,Ord,Show)

type Decls a = Decl Expr a

data Decl f a = Decls [(String, Scope Int f a)]
  deriving (Eq,Foldable,Functor,Ord,Read,Show,Traversable)

instance Bound Decl where
    Decls x >>>= f = Decls $ map (second (>>>= f)) x

abstractKeys :: Eq a => [(a, b)] -> Expr a -> Scope Int Expr a
abstractKeys names = abstr
  where
    vs = map fst names
    abstr = abstract (`elemIndex` vs)

buildDecls :: [(String, Expr String)] -> Decls String
buildDecls namedExprs =
    Decls $ map (second (abstractKeys namedExprs)) namedExprs

buildLet :: Decls String -> Expr String -> Loc -> Expr String
buildLet decls@(Decls defs) e = Let decls (abstractKeys defs e)

data Expr a
  = App (Expr a) (Expr a) Loc
  | If (Expr a) (Expr a) (Expr a) Loc
  | Let (Decls a) (Scope Int Expr a) Loc
  | Lambda String Type (Scope () Expr a) Loc
  | Bool Bool Loc
  | Int Integer Loc
  | Var a Loc
  deriving (Eq,Foldable,Functor,Ord,Show,Traversable)

instance Eq1 Expr
instance Ord1 Expr
instance Show1 Expr

instance Applicative Expr where
    pure = return
    (<*>) = ap

instance Monad Expr where
    return x = Var x Undefined
    App e1 e2 loc          >>= f = App (e1 >>= f) (e2 >>= f) loc
    Lambda name t body loc >>= f = Lambda name t (body >>>= f) loc
    Let decls body loc     >>= f = Let (decls >>>= f) (body >>>= f) loc
    Int i loc              >>= _ = Int i loc
    Bool b loc             >>= _ = Bool b loc
    Var x _                >>= f = f x
    If p x y loc           >>= f = If (p >>= f) (x >>= f) (y >>= f) loc
