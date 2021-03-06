{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module TypeCheck (typeCheckModule, typeCheckExpr) where

import Prelude hiding (lookup, span)
import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor
import Data.Either.Validation

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import AST
import Eval
import PrettyPrint
import UniqMap

type TC a = StateT (UniqMap Name Type) (Except Doc) a

rule :: Const -> Const -> TC Const
rule Star Star = return Star
rule Star Box  = throwError $ text "No dependent types!"
rule Box  Star = return Star
rule Box  Box  = return Box

typeCheckModule :: Module (Named Type) -> Either Doc ()
typeCheckModule m = runExcept $ evalStateT (checkDecls (m^.decls)) conTypes
  where
    conTypes = Type <$> m^.cons

lookupCons :: Name -> Loc -> TC Type
lookupCons n l = preuse (ix n) >>= \case
    Just t -> return . set location l $ t
    Nothing -> throwError $ vsep
        [ prettyLoc l <> text ":" <+> red (text "error") <> text ":"
        , prettySource l
        , empty
        , string "Constructor name" <+> bold (prettify n)
            <+> string "not defined!"
        ]

typeCheckExpr
    :: Module (Named Type)
    -> Scope NamedVar Expr (Named Type)
    -> Either Doc Type
typeCheckExpr m expr = runExcept $ evalStateT checkType conTypes
  where
    conTypes = Type <$> m^.cons
    checkType = do
        decs <- instDecls $ m^.decls
        typeOf $ instantiate (fst . exprAt decs) expr


unify :: Type -> Type -> TC ()
unify (Type t1) (Type t2) = when (t1 /= t2) $ do
    throwError . vsep $
        [ string "Could not match:"
        , prettyLoc (t1^.location)
        , prettySource (t1^.location)
        , prettify t1
        , string "With:"
        , prettyLoc (t2^.location)
        , prettySource (t2^.location)
        , prettify t2
        ]

instDecls :: Decls (Named Type) -> TC (UniqMap Name (Expr (Named Type), Type))
instDecls decs = case result of
    Failure _ -> throwError $ text "Whoops"
    Success r -> return r
  where
    es = fmap (first instType) decs
    vs = instantiateDecls decs

    instType :: Scope NamedVar Expr (Named Type) -> Expr (Named Type)
    instType = instantiateNamed (Type . snd . exprAt vs)

    result = intersectUniq (\(e, _) (_, t) -> (e, Type t))  es vs

checkDecls :: Decls (Named Type) -> TC ()
checkDecls = instDecls >=> mapM_ checkPair
  where
    checkPair (e, ty) = do
        t1 <- typeOf e
        unify ty t1

typeWith :: Pat a -> Type -> TC [Type]
typeWith (Simple (VarP _ l)) t = return [set location l t]
typeWith (Simple (WildP _)) _ = return []
typeWith (AsP _ p l) t = (set location l t:) <$> typeWith p t
typeWith (ConP s pats l) t = do
    conTy <- lookupCons s l
    (rt, binds) <- matchType conTy pats
    unify t rt
    return binds
  where
    matchType :: Type -> [Pat a] -> TC (Type, [Type])
    matchType (Type (Pi _ t1 t2 _)) (p:ps) = do
        r1 <- typeWith p (Type t1)
        (rt, r2) <- matchType (Type $ instantiate1 t1 t2) ps
        return (rt, r1 ++ r2)
    matchType rt [] = return (rt, [])
    matchType _ _ = throwError $
        string "Wrong number of arguments in pattern:" <> line <>
        prettySource l

typeOf :: Expr (Named Type) -> TC Type
typeOf (Const Star l) = return . Type $ Const Box l
typeOf (Const Box _) = throwError $ text "BOX is not typeable"
typeOf (Var ty) = return $ stripName ty
typeOf (LocVar ty _) = return $ stripName ty
typeOf (Con s _ l) = lookupCons s l

typeOf (Lambda pat ty body l) = do
    Type t1 <- typeOf $ instantiateNamed (const (Type ty)) body
    let piType = Pi pat ty (abstractPat pat t1) l
    _ <- typeOf piType
    return $ Type piType

typeOf (Pi _ ty body l) = do
    Type t1 <- typeOf ty
    Type t2 <- typeOf $ instantiateNamed (const (Type ty)) body
    case (whnf t1, whnf t2) of
        (Const c1 _, Const c2 _) -> (\c -> Type (Const c l)) <$> rule c1 c2
        _ -> throwError . string $ "Type error in Pi type!"

typeOf (App e1 e2 _) = do
    Type t1 <- typeOf e1
    (Pi _ ty body _) <- case whnf t1 of
        t@Pi{} -> return t
        _ -> do
          throwError . vsep $
            [ string "Function does not have Pi type!"
            , string (show e1)
            , prettyLoc (e1^.location)
            , prettySource (e1^.location)
            , string "Applied to:"
            , prettyLoc (e2^.location)
            , prettySource (e2^.location)
            , string "Inferred type:" <+> prettify t1
            , prettyLoc (t1^.location)
            , prettySource (t1^.location)
            ]
    Type t2 <- typeOf e2
    if nf ty == nf t2
       then return . Type $ instantiate1 e2 body
       else throwError . vsep $
            [ string "Wrong application!"
            , string "Inferred argument type:"
            , prettify ty
            , prettyLoc (ty^.location)
            , prettySource (ty^.location)
            , string "Found argument type:"
            , prettify t2
            , prettyLoc (t2^.location)
            , prettySource (t2^.location)
            ]

typeOf (Case e alts _) = do
    t1 <- typeOf e
    (Type t:types) <- forM alts $ \(Alt pat body _) -> do
        ts <- pat `typeWith` t1
        typeOf $ instantiateNamed (ts!!) body
    mapM_ (\(Type x) -> unify (Type $ nf t) (Type $ nf x)) types
    return $ Type t

typeOf (Let decs body _) = do
    checkDecls decs
    d <- instDecls decs
    typeOf $ instantiateNamed (snd . exprAt d) body
