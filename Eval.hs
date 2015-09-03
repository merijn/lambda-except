module Eval where

import Bound

import AST

instantiateDecls :: Decls a -> Scope Int Expr a -> Expr a
instantiateDecls (Decls decls) = inst
  where
    es = map (inst . snd) decls
    inst = instantiate (es !!)

whnfWith :: Decls String -> Expr String -> Expr String
whnfWith decls e = whnf $ buildLet decls e Undefined

nfWith :: Decls String -> Expr String -> Expr String
nfWith decls e = nf $ buildLet decls e Undefined

whnf :: Expr a -> Expr a
whnf (App t1 t2 l)
    | Lambda _ _ body _ <- fun = whnf $ instantiate1 t2 body
    | otherwise = App fun t2 l
    where
      fun = whnf t1

whnf (If p x y l)
    | Bool b _ <- predicate = if b then whnf x else whnf y
    | otherwise = If predicate x y l
    where
      predicate = whnf p

whnf (Let decls body _) = whnf $ instantiateDecls decls body

whnf expr@Lambda{} = expr
whnf expr@Bool{} = expr
whnf expr@Int{} = expr
whnf expr@Var{} = expr

nf :: Expr a -> Expr a
nf (App t1 t2 l) = case whnf t1 of
    Lambda _ _ body _ -> nf $ instantiate1 t2 body
    t1' -> App (nf t1') (nf t2) l

nf (If p x y l) = case nf p of
    Bool b _ -> if b then nf x else nf y
    predicate -> If predicate (nf x) (nf y) l

nf (Let decls body _) = nf $ instantiateDecls decls body
nf (Lambda n t body l) = Lambda n t (toScope . nf . fromScope $ body) l
nf expr@Bool{} = expr
nf expr@Int{} = expr
nf expr@Var{} = expr
