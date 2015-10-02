{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module AST
    ( Type(..)
    , getResultType
    , DataType(..)
    , Module(..)
    , Name(..)
    , Named(..)
    , NamedType
    , NamedVar
    , Decls
    , Expr(..)
    , Pat(..)
    , Alt(..)
    , abstractKeys
    , abstractPos
    , instantiate
    , instantiateNamed
    , abstract1
    , stripName
    , Span(..)
    , span
    , spans
    , HasDataType(..)
    , HasModule(..)
    , module Bound
    ) where

import Prelude hiding (span)
import Prelude.Extras

import Bound hiding (instantiate, abstract1)
import qualified Bound
import Control.Lens
import Control.Monad
import Data.Foldable (toList)
import Data.List (elemIndex)
import Text.Trifecta.Rendering (Span(..), HasSpan(..))

import UniqMap

data Name = Name String Span deriving (Show)

instance Eq Name where
    Name n1 _ == Name n2 _ = n1 == n2

instance Ord Name where
    Name n1 _ `compare` Name n2 _ = n1 `compare` n2

instance HasSpan Name where
    span f (Name n s) = Name n <$> f s

data Named a = Named Name a
    deriving (Eq,Foldable,Functor,Ord,Show,Traversable)

type NamedType = Named Type
type NamedVar = Named Int

data Type = TyCon String Span | TyArr Type Type Span | TyAny Span
    deriving (Eq,Ord,Show)

getResultType :: Type -> Type
getResultType (TyArr _ t _) = getResultType t
getResultType t = t

data DataType = Data
    { _dataName :: Name
    , _dataCons :: UniqMap Name Type
    }
    deriving (Eq,Ord,Show)

data Module a = Module
    { _modTypes :: UniqMap Name DataType
    , _modCons :: UniqMap Name (Type, DataType)
    , _modDecls :: Decls a
    }
    deriving (Eq,Foldable,Functor,Ord,Show,Traversable)

type Decls a = UniqMap Name (Scope NamedVar Expr a, Type)

data Expr a
  = App (Expr a) (Expr a) Span
  | Let (Decls a) (Scope NamedVar Expr a) Span
  | Lambda Name (Maybe Type) (Scope (Named ()) Expr a) Span
  | Case (Expr a) [Alt Expr a] Span
  | Con Name [Expr a] Span
  | Int Integer Span
  | LocVar a Span
  | Var a
  deriving (Eq,Foldable,Functor,Ord,Show,Traversable)

instance Eq1 Expr
instance Ord1 Expr
instance Show1 Expr

instance Applicative Expr where
    pure = return
    (<*>) = ap

instance Monad Expr where
    return = Var
    App e1 e2 loc           >>= f = App (e1 >>= f) (e2 >>= f) loc
    Lambda i names body loc >>= f = Lambda i names (body >>>= f) loc
    Let decls body loc      >>= f = Let (helper <$> decls) (body >>>= f) loc
      where helper (e, ty) = (e >>>= f, ty)
    Case e alts loc         >>= f = Case (e >>= f) (map (>>>= f) alts) loc
    Con s args loc          >>= f = Con s (map (>>=f) args) loc
    Int i loc               >>= _ = Int i loc
    LocVar x _              >>= f = f x
    Var x                   >>= f = f x

data Pat a
  = VarP a Span
  | WildP Span
  | AsP a (Pat a) Span
  | ConP Name [Pat a] Span
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

data Alt f a = Alt (Pat Name) (Scope NamedVar f a) Span
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

instance Bound Alt where
  Alt p b l >>>= f = Alt p (b >>>= f) l

nameIt :: Functor f => (Name -> f a) -> Name -> f (Named a)
nameIt f n = Named n <$> f n

abstractKeys :: UniqMap Name v -> Expr Name -> Scope NamedVar Expr Name
abstractKeys names = abstract $ nameIt (lookupName names)

abstractPos :: Foldable f => f Name -> Expr Name -> Scope NamedVar Expr Name
abstractPos names = abstract $ nameIt (`elemIndex` toList names)

instantiate :: Monad f => (b -> f a) -> Scope (Named b) f a -> f a
instantiate f = Bound.instantiate (f . stripName)

instantiateNamed
    :: Monad f => (b -> a) -> Scope (Named b) f (Named a) -> f (Named a)
instantiateNamed f = Bound.instantiate (pure . fmap f)

abstract1 :: (Monad f) => Name -> f Name -> Scope (Named ()) f Name
abstract1 x = abstract checkName
  where
    checkName y | x == y = Just $ Named y ()
                | otherwise = Nothing

stripName :: Named a -> a
stripName (Named _ x) = x

class MightHaveSpan a where
    spans :: Traversal' a Span

instance MightHaveSpan (Expr a) where
    spans f expr = case expr of
        App e1 e2 l -> App e1 e2 <$> f l
        Let binds body l -> Let binds body <$> f l
        Lambda name ty body l -> Lambda name ty body <$> f l
        Case e alts l -> Case e alts <$> f l
        Con s args l -> Con s args <$> f l
        Int i l -> Int i <$> f l
        LocVar x l -> LocVar x <$> f l
        Var x -> pure $ Var x

instance HasSpan Type where
    span f ty = case ty of
        TyAny l -> TyAny <$> f l
        TyCon s l -> TyCon s <$> f l
        TyArr t1 t2 l -> TyArr t1 t2 <$> f l

makeClassy ''DataType
makeClassyFor "HasModule" "module_" [("_modTypes", "modTypes"),
    ("_modCons", "modCons"), ("_modDecls", "modDecls")] ''Module
