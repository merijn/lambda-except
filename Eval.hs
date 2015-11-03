{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Eval where

import Data.Bifunctor
import Data.Maybe

import AST
import UniqMap

instantiateDecls :: Decls a -> Scope NamedVar Expr a -> Expr a
instantiateDecls decs = inst
  where
    es = fmap (first inst) decs
    inst = instantiate (fst . exprAt es)

matchPatWithEval
    :: forall a . (Expr a -> Expr a)
    -> Expr a
    -> Alt Expr a
    -> Maybe (Expr a)
matchPatWithEval eval expr (Alt pat body _) = inst . snd <$> match pat expr
  where
    inst :: [Expr a] -> Expr a
    inst bindings = instantiate ((bindings !!)) body

    match :: Pat Name -> Expr a -> Maybe (Expr a, [Expr a])
    match (Simple (VarP _ _)) e = Just (e, [e])
    match (Simple WildP{}) e = Just (e, [])
    match (AsP _ pats _) e
      | Just (e', ms) <- match pats e = Just (e', e':ms)

    match (ConP s1 pats _) (eval -> Con s2 args l)
      | s1 == s2
      , Just (es, ms) <- go pats args = Just (Con s2 es l, ms)
      where
        go :: [Pat Name] -> [Expr a] -> Maybe ([Expr a], [Expr a])
        go [] [] = Just ([], [])
        go (p:ps) (a:as)
            | Just (e, ms) <- match p a
            = bimap (e:) (ms++) <$> go ps as
        go _ _ = Nothing

    match _ _ = Nothing

whnfWith :: Module Name -> Expr Name -> Expr Name
whnfWith m e = whnf $ instantiateDecls decs (abstractKeys decs e)
  where decs = m^.decls

nfWith :: Module Name -> Expr Name -> Expr Name
nfWith m e = nf $ instantiateDecls decs (abstractKeys decs e)
  where decs = m^.decls

whnf :: Expr a -> Expr a
whnf expr@Const{} = expr
whnf expr@Var{} = expr
whnf expr@LocVar{} = expr
whnf expr@Con{} = expr
whnf expr@Lambda{} = expr
whnf expr@Pi{} = expr

whnf (App t1 t2 l)
    | Lambda _ _ body _ <- fun = whnf $ instantiate1 t2 body
    | (Con s es loc) <- fun = Con s (es ++ [t2]) loc
    | otherwise = App fun t2 l
    where
      fun = whnf t1

whnf (Case expr alts _) =
  case mapMaybe (matchPatWithEval whnf expr) alts of
    [] -> error "Incomplete pattern!"
    (x:_) -> x

whnf (Let decs body _) = whnf $ instantiateDecls decs body

nf :: Expr a -> Expr a
nf expr@Const{} = expr
nf expr@Var{} = expr
nf expr@LocVar{} = expr
nf (Con s args l) = Con s (map nf args) l
nf (Lambda n t body l) = Lambda n t (toScope . nf . fromScope $ body) l
nf (Pi n t1 t2 l) = Pi n (nf t1) (toScope . nf . fromScope $ t2) l

nf (App t1 t2 l) = case whnf t1 of
    Lambda _ _ body _ -> nf $ instantiate1 t2 body
    Con s es loc -> nf $ Con s (es ++ [t2]) loc
    t1' -> App (nf t1') (nf t2) l

nf (Case (nf -> expr) alts@(Alt pat _ _:_) l)
    | Var{} <- expr
    , notWildCard pat = Case expr alts l
  where
    notWildCard (Simple WildP{}) = False
    notWildCard _ = True

nf (Case (nf -> expr) alts _) =
  case mapMaybe (matchPatWithEval id expr) alts of
    [] -> error "Incomplete pattern!"
    (x:_) -> nf x

nf (Let decs body _) = nf $ instantiateDecls decs body
