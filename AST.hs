{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module AST
    ( Name(..)
    , Named(..)
    , NamedVar
    , Type(..)
    , Data(..)
    , Module(..)
    , Decls
    , Const(..)
    , Expr(..)
    , SimplePat(..)
    , Pat(..)
    , Alt(..)
    , isCompoundExpr
    , abstractKeys
    , abstractPos
    , instantiate
    , instantiateNamed
    , instantiateDecls
    , instantiateExpr
    , abstractPat
    , stripName
    , HasName(..)
    , HasType(..)
    , HasCons(..)
    , HasDatas(..)
    , HasDecls(..)
    , Span(..)
    , HasSpan(..)
    , Loc(..)
    , HasLoc(..)
    , module Bound
    , module Control.Lens
    ) where

import Prelude hiding (span)
import Prelude.Extras

import Bound hiding (instantiate)
import qualified Bound
import Control.Lens hiding (Const, cons)
import Control.Monad
import Data.Foldable (toList)
import Data.List (elemIndex)
import Text.Trifecta.Rendering (Span(..), HasSpan(..))

import UniqMap

newtype Loc = Loc Span

instance Show Loc where
    show _ = ""

instance Eq Loc where
    _ == _ = True

instance Ord Loc where
    compare _ _ = EQ

data Name = Name String Loc deriving (Show)

data Named a = Named Name a
    deriving (Eq,Foldable,Functor,Ord,Show,Traversable)

type NamedVar = Named Int

newtype Type = Type (Expr (Named Type))
    deriving (Eq,Show)

data Data a = Data
    { _dataName :: Name
    , _dataKind :: Expr a
    , _dataCons :: UniqMap Name (Expr a)
    }
    deriving (Eq,Foldable,Functor,Ord,Show,Traversable)

data Module a = Module
    { _modDatas :: UniqMap Name (Data a)
    , _modCons :: UniqMap Name (Expr a)
    , _modDecls :: Decls a
    }
    deriving (Eq,Foldable,Functor,Ord,Show,Traversable)

type Decls a = UniqMap Name (Scope NamedVar Expr a, Scope NamedVar Expr a)

data Const = Star | Box deriving (Eq,Ord,Show)

data Expr a
  = Const Const Loc
  | Var a
  | LocVar a Loc
  | Con Name [Expr a] Loc
  | Lambda (SimplePat Name) (Expr a) (Scope (Named ()) Expr a) Loc
  | Pi (SimplePat Name) (Expr a) (Scope (Named ()) Expr a) Loc
  | App (Expr a) (Expr a) Loc
  | Case (Expr a) [Alt Expr a] Loc
  | Let (Decls a) (Scope NamedVar Expr a) Loc
  deriving (Foldable,Functor,Ord,Show,Traversable)

data SimplePat a
  = WildP Loc
  | VarP a Loc
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

data Pat a
  = Simple (SimplePat a)
  | AsP a (Pat a) Loc
  | ConP Name [Pat a] Loc
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

data Alt f a = Alt (Pat Name) (Scope NamedVar f a) Loc
  deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

-- Functions

isCompoundExpr :: Expr a -> Bool
isCompoundExpr e = case e of
    Const{} -> False
    Var{} -> False
    LocVar{} -> False
    Con _ [] _ -> False
    _ -> True

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

instantiateDecls :: Decls a -> UniqMap Name (Expr a, Expr a)
instantiateDecls decs = vs
  where
    vs = fmap (bimap instVal instVal) decs
    instVal = instantiate (fst . exprAt vs)

instantiateExpr :: Decls a -> Scope NamedVar Expr a -> Expr a
instantiateExpr decs = instantiate (fst . exprAt (instantiateDecls decs))

abstractPat
    :: (HasName a, Monad f)
    => SimplePat Name
    -> f a
    -> Scope (Named ()) f a
abstractPat WildP{} = abstract (const Nothing)
abstractPat (VarP n _) = abstract checkName
  where
    checkName (view name -> y)
        | n == y = Just $ Named y ()
        | otherwise = Nothing

stripName :: Named a -> a
stripName (Named _ x) = x

-- Classes

class HasLoc a where
    location :: Lens' a Loc

class HasName a where
    name :: Lens' a Name

class HasType f where
    type_ :: Lens' (f a) (Expr a)

class HasCons f where
    cons :: Lens' (f a) (UniqMap Name (Expr a))

class HasDatas f where
    datas :: Lens' (f a) (UniqMap Name (Data a))

class HasDecls f where
    decls :: Lens' (f a) (Decls a)

-- Instances

instance HasSpan Loc where
    span f (Loc s) = Loc <$> f s

instance HasLoc Loc where
    location = id



instance Eq Name where
    Name n1 _ == Name n2 _ = n1 == n2

instance Ord Name where
    Name n1 _ `compare` Name n2 _ = n1 `compare` n2

instance HasName Name where
    name = id

instance HasLoc Name where
    location f (Name n s) = Name n <$> f s



instance HasName (Named a) where
    name f (Named n x) = (\n' -> Named n' x) <$> f n

instance HasLoc a => HasLoc (Named a) where
    location f (Named n x) = Named n <$> location f x



instance HasLoc Type where
    location f (Type e) = Type <$> location f e



instance HasName (Data a) where
    name f t = (\n -> t { _dataName = n }) <$> f (_dataName t)

instance HasType Data where
    type_ f t = (\e -> t { _dataKind = e }) <$> f (_dataKind t)

instance HasCons Data where
    cons f t = (\cs -> t { _dataCons = cs }) <$> f (_dataCons t)



instance HasCons Module where
    cons f t = (\cs -> t { _modCons = cs }) <$> f (_modCons t)

instance HasDatas Module where
    datas f t = (\cs -> t { _modDatas = cs }) <$> f (_modDatas t)

instance HasDecls Module where
    decls f t = (\cs -> t { _modDecls = cs }) <$> f (_modDecls t)



instance Eq1 Expr
instance Ord1 Expr
instance Show1 Expr

instance Eq a => Eq (Expr a) where
    Const c1 _ == Const c2 _ = c1 == c2
    Var x == Var y = x == y
    LocVar x _ == LocVar y _ = x == y
    Var x == LocVar y _ = x == y
    LocVar x _ == Var y = x == y
    Con n1 es1 _ == Con n2 es2 _ = n1 == n2 && es1 == es2
    Lambda _ ty1 body1 _ == Lambda _ ty2 body2 _ = ty1 == ty2 && body1 == body2
    Pi _ ty1 body1 _ == Pi _ ty2 body2 _ = ty1 == ty2 && body1 == body2
    App e11 e12 _ == App e21 e22 _ = e11 == e21 && e12 == e22
    Case e1 alts1 _ == Case e2 alts2 _ = e1 == e2 && alts1 == alts2
    Let decls1 body1 _ == Let decls2 body2 _ = decls1 == decls2 && body1 == body2
    _ == _ = False

instance Applicative Expr where
    pure = return
    (<*>) = ap

instance Monad Expr where
    return = Var
    Const c loc          >>= _ = Const c loc
    Var x                >>= f = f x
    LocVar x _           >>= f = f x
    Con s args loc       >>= f = Con s (map (>>=f) args) loc
    Lambda n ty body loc >>= f = Lambda n (ty >>= f) (body >>>= f) loc
    Pi n ty body loc     >>= f = Pi n (ty >>= f) (body >>>= f) loc
    App e1 e2 loc        >>= f = App (e1 >>= f) (e2 >>= f) loc
    Case e alts loc      >>= f = Case (e >>= f) (map (>>>= f) alts) loc
    Let decs body loc   >>= f = Let (go <$> decs) (body >>>= f) loc
      where go (e, ty) = (e >>>= f, ty >>>= f)

instance HasLoc a => HasLoc (Expr a) where
    location f expr = case expr of
        Const c l -> Const c <$> f l
        Var x -> LocVar x <$> f (x^.location)
        LocVar x l -> LocVar x <$> f l
        Con s args l -> Con s args <$> f l
        Lambda n ty body l -> Lambda n ty body <$> f l
        Pi n ty body l -> Pi n ty body <$> f l
        App e1 e2 l -> App e1 e2 <$> f l
        Case e alts l -> Case e alts <$> f l
        Let binds body l -> Let binds body <$> f l



instance HasLoc (SimplePat a) where
    location f pat = case pat of
        WildP l -> WildP <$> f l
        VarP n l -> VarP n <$> f l

instance HasLoc (Pat a) where
    location f pat = case pat of
        Simple p -> Simple <$> location f p
        AsP n p l -> AsP n p <$> f l
        ConP n ps l -> ConP n ps <$> f l



instance Bound Alt where
  Alt p b l >>>= f = Alt p (b >>>= f) l
