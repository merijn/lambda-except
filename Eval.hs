{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Eval where

import Bound
import Data.Bifunctor
import Data.Maybe

import AST
import UniqMap

instantiateDecls :: Decls a -> Scope Int Expr a -> Expr a
instantiateDecls decls = inst
  where
    es = fmap (first inst) decls
    inst = instantiate (fst . exprAt es)

matchPatWithEval
    :: forall a . (Expr a -> Expr a)
    -> Expr a
    -> Alt Expr a
    -> Maybe (Expr a)
matchPatWithEval eval expr (Alt pat body _) = inst . snd <$> match pat expr
  where
    inst :: [Expr a] -> Expr a
    inst bindings = instantiate (bindings !!) body

    match :: Pat String -> Expr a -> Maybe (Expr a, [Expr a])
    match (VarP _ _) e = Just (e, [e])
    match WildP{} e = Just (e, [])
    match (AsP _ pats _) e
      | Just (e', ms) <- match pats e = Just (e', e':ms)

    match (ConP s1 pats _) (eval -> Con s2 args l)
      | s1 == s2
      , Just (es, ms) <- go pats args = Just (Con s2 es l, ms)
      where
        go :: [Pat String] -> [Expr a] -> Maybe ([Expr a], [Expr a])
        go [] [] = Just ([], [])
        go (p:ps) (a:as)
            | Just (e, ms) <- match p a
            = bimap (e:) (ms++) <$> go ps as
        go _ _ = Nothing

    match _ _ = Nothing

whnfWith :: Module String -> Expr String -> Expr String
whnfWith m e = whnf $ Let decls (abstractKeys decls e) Builtin
  where decls = moduleDecls m

nfWith :: Module String -> Expr String -> Expr String
nfWith m e = nf $ Let decls (abstractKeys decls e) Builtin
  where decls = moduleDecls m

whnf :: Expr a -> Expr a
whnf (App t1 t2 l)
    | Lambda _ _ body _ <- fun = whnf $ instantiate1 t2 body
    | otherwise = App fun t2 l
    where
      fun = whnf t1

whnf (Case expr alts _) =
  case mapMaybe (matchPatWithEval whnf expr) alts of
    [] -> error "Incomplete pattern!"
    (x:_) -> x

whnf (Let decls body _) = whnf $ instantiateDecls decls body

whnf expr@Lambda{} = expr
whnf expr@Con{} = expr
whnf expr@Int{} = expr
whnf expr@Var{} = expr

nf :: Expr a -> Expr a
nf (App t1 t2 l) = case whnf t1 of
    Lambda _ _ body _ -> nf $ instantiate1 t2 body
    t1' -> App (nf t1') (nf t2) l

nf (Case (nf -> expr) alts@(Alt pat _ _:_) l)
    | Var{} <- expr
    , notWildCard pat = Case expr alts l
  where
    notWildCard WildP{} = False
    notWildCard _ = True

nf (Case (nf -> expr) alts _) =
  case mapMaybe (matchPatWithEval id expr) alts of
    [] -> error "Incomplete pattern!"
    (x:_) -> x

nf (Let decls body _) = nf $ instantiateDecls decls body
nf (Lambda n t body l) = Lambda n t (toScope . nf . fromScope $ body) l
nf (Con s args l) = Con s (map nf args) l
nf expr@Int{} = expr
nf expr@Var{} = expr
