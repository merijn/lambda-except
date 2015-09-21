{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
module AST
    ( Loc(..)
    , Type(..)
    , DataType(..)
    , Module(..)
    , Decls
    , Expr(..)
    , Pat(..)
    , Alt(..)
    , abstractKeys
    , abstractPos
    ) where

import Prelude.Extras

import Bound
import Control.Monad
import Data.Foldable (toList)
import Data.List (elemIndex)
import Text.Trifecta.Delta (Delta)

import UniqMap

data Loc = Undefined | Loc Delta Delta | Builtin
    deriving (Eq,Ord,Show)

data Type = TyCon String Loc | TyArr Type Type Loc | TyAny
    deriving (Eq,Ord,Show)

data DataType = Data
    { dataName :: String
    , dataCons :: UniqMap String Type
    }
    deriving (Eq,Ord,Show)

data Module a = Module
    { moduleTypes :: UniqMap String DataType
    , moduleCons :: UniqMap String (Type, DataType)
    , moduleDecls :: Decls a
    }
    deriving (Eq,Foldable,Functor,Ord,Show,Traversable)

type Decls a = UniqMap String (Scope Int Expr a, Type)

data Expr a
  = App (Expr a) (Expr a) Loc
  | Let (Decls a) (Scope Int Expr a) Loc
  | Lambda String Type (Scope () Expr a) Loc
  | Case (Expr a) [Alt Expr a] Loc
  | Con String [Expr a] Loc
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
    Let decls body loc     >>= f = Let (helper <$> decls) (body >>>= f) loc
      where helper (e, ty) = (e >>>= f, ty)
    Case e alts loc        >>= f = Case (e >>= f) (map (>>>= f) alts) loc
    Con s args loc         >>= f = Con s (map (>>=f) args) loc
    Int i loc              >>= _ = Int i loc
    Var x _                >>= f = f x

data Pat a
  = VarP a Loc
  | WildP Loc
  | AsP a (Pat a) Loc
  | ConP String [Pat a] Loc
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

data Alt f a = Alt (Pat String) (Scope Int f a) Loc
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

instance Bound Alt where
  Alt p b l >>>= f = Alt p (b >>>= f) l

abstractKeys :: Ord k => UniqMap k v -> Expr k -> Scope Int Expr k
abstractKeys names = abstract (lookupName names)

abstractPos :: (Eq k, Foldable f) => f k -> Expr k -> Scope Int Expr k
abstractPos names = abstract (`elemIndex` toList names)
