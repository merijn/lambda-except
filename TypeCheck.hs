{-# LANGUAGE LambdaCase #-}
module TypeCheck (typeCheckModule, typeCheckExpr) where

import Prelude hiding (lookup, span)
import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import AST
import PrettyPrint
import UniqMap

type TC a = StateT (Module NamedType) (Except Doc) a

typeCheckModule :: Module NamedType -> Either Doc ()
typeCheckModule m = runExcept . flip evalStateT m $ do
    checkCons (m^.modCons)
    checkDecls (m^.modDecls)

lookupCons :: Name -> Span -> TC Type
lookupCons n l = preuse (modCons . ix n . _1) >>= \case
    Just x -> return x
    Nothing -> throwError $ vsep
        [ prettySpan l <> text ":" <+> red (text "error") <> text ":"
        , prettySource l
        , empty
        , string "Constructor name" <+> bold (prettify n)
            <+> string "not defined!"
        ]

typeCheckExpr
    :: Module NamedType
    -> Scope NamedVar Expr NamedType
    -> Either Doc Type
typeCheckExpr m expr = runExcept $ evalStateT (typeOf (unScope expr)) m
  where
    unScope = instantiateNamed (snd . exprAt (m^.modDecls))

unify :: Type -> Type -> (Doc -> Doc) -> TC Type
unify (TyArr t11 t12 l) (TyArr t21 t22 _) err =
    TyArr <$> unify t11 t21 err <*> unify t12 t22 err <*> pure l

unify t@(TyCon s1 _) (TyCon s2 _) _ | s1 == s2 = pure t
unify TyAny{} t _ = pure t
unify t TyAny{} _ = pure t
unify t1 t2 err = throwError . err . vsep $
    [ string "Could not match:"
    , prettySpan (t1^.span)
    , prettySource (t1^.span)
    , string "Inferred type:" <+> prettify t1

    , string "with:"
    , prettySpan (t1^.span)
    , prettySource (t2^.span)
    , string "Inferred type:" <+> prettify t2
    ]

checkCons :: UniqMap Name (Type, DataType) -> TC ()
checkCons cs = forM_ cs $ \(ty, dataType_) -> do
    let (Name n s) = dataType_^.dataName
    unify (TyCon n s) (getResultType ty) id

checkDecls :: Decls NamedType -> TC ()
checkDecls decls = forM_ ds $ \(e, ty) -> do
    t <- typeOf e
    unify t ty id
  where
    ds = fmap (first inst) decls
    inst = instantiateNamed (snd . exprAt ds)

typePat :: Pat a -> TC (Type, [Type])
typePat (VarP _ l) = return $ (TyAny l, [TyAny l])
typePat (WildP l) = return $ (TyAny l, [])
typePat (AsP _ p _) = typePat p >>= return . \(ty, tys) -> (ty, ty:tys)
typePat (ConP s ps l) = do
    conTy <- lookupCons s l
    (types, argTypes) <- unzip <$> mapM typePat ps
    result <- matchType conTy types
    return (result, concat argTypes)
  where
    matchType :: Type -> [Type] -> TC Type
    matchType t@(TyCon _ _) [] = return t
    matchType (TyArr t1 t2 _) (t:ts) = unify t1 t id >> matchType t2 ts
    matchType _ _ = throwError $
        string "Wrong number of arguments in pattern:" <> line <>
        prettySource l

typeOf :: Expr NamedType -> TC Type
typeOf (App e1 e2 l) = do
    typeOf e1 >>= \case
        TyArr t11 t12 _ -> do
            t2 <- typeOf e2
            unify t11 t2 id
            return $ (span.~l) t12
        ty -> throwError $ string "Not a function type" <> line <> prettify ty

typeOf (Let decls body _) = do
    checkDecls decls
    typeOf $ instantiateNamed (snd . exprAt decls) body

typeOf (Case e alts _) = do
    t1 <- typeOf e
    (t:types) <- forM alts $ \(Alt pat body _) -> do
        (t2, ts) <- typePat pat
        unify t1 t2 id
        typeOf $ instantiateNamed (ts!!) body
    mapM_ (\x -> unify t x id) types
    return t

typeOf (Lambda n ty body l) = do
    resultType <- typeOf (instantiate1 (pure $ Named n varType) body)
    return $ TyArr varType resultType l
  where
    varType | Just t <- ty = t
            | otherwise = TyAny $ n^.span

typeOf (Con s _ l) = lookupCons s l
typeOf (Int _ l) = return $ TyCon "Int" l
typeOf (LocVar ty _) = return $ stripName ty
typeOf (Var ty) = return $ stripName ty
