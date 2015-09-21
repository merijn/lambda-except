module TypeCheck (typeCheckModule, typeCheckExpr) where

import Bound
import Control.Monad.Except
import Control.Monad.State
import Data.Bifunctor

import AST
import UniqMap

type TC a = StateT (Module Type) (Except String) a

typeCheckModule :: Module Type -> Either String ()
typeCheckModule m = runExcept $ evalStateT (checkDecls (moduleDecls m)) m

typeCheckExpr :: Module Type -> Scope Int Expr Type -> Either String Type
typeCheckExpr m expr = runExcept $ evalStateT (typeOf (unScope expr)) m
  where
    unScope = instantiate (pure . snd . exprAt (moduleDecls m))

unify :: Type -> Type -> Bool
unify (TyArr t11 t12 _) (TyArr t21 t22 _) = unify t11 t21 && unify t12 t22
unify (TyCon s1 _) (TyCon s2 _) = s1 == s2
unify TyAny _ = True
unify _ TyAny = True
unify _ _ = False

checkDecls :: Decls Type -> TC ()
checkDecls decls = forM_ ds $ \(e, ty) -> do
    t <- typeOf e
    when (not (unify t ty)) $ throwError "Type error!"
  where
    ds = fmap (first inst) decls
    inst = instantiate (pure . snd . exprAt ds)

typePat :: Pat a -> TC (Type, [Type])
typePat VarP{} = return $ (TyAny, [TyAny])
typePat WildP{} = return $ (TyAny, [])
typePat (AsP _ p _) = typePat p >>= return . \(ty, tys) -> (ty, ty:tys)
typePat (ConP s ps _) = do
    conTy <- gets (fst . (!s) . moduleCons)
    (types, argTypes) <- unzip <$> mapM typePat ps
    result <- matchType conTy types
    return (result, concat argTypes)
  where
    matchType :: Type -> [Type] -> TC Type
    matchType t@(TyCon _ _) [] = return t
    matchType (TyArr t1 t2 _) (t:ts)
        | unify t1 t = matchType t2 ts
        | otherwise = throwError "Type error"
    matchType _ _ = throwError "Type error"

typeOf :: Expr Type -> TC Type
typeOf (App e1 e2 _) = do
    (TyArr t11 t12 _) <- typeOf e1
    t21 <- typeOf e2
    if unify t11 t21
       then return $ t12
       else throwError "Type error"

typeOf (Let decls body _) = do
    checkDecls decls
    typeOf $ instantiate (pure . snd . exprAt decls) body

typeOf (Case e alts _) = do
    t1 <- typeOf e
    (t:types) <- forM alts $ \(Alt pat body _) -> do
        (t2, ts) <- typePat pat
        when (not (unify t1 t2)) $ throwError "Type error"
        typeOf $ instantiate (pure . (ts!!)) body
    when (any (not . unify t) types) $ throwError "Type error"
    return t

typeOf (Lambda _ ty body l) = do
    resultType <- typeOf (instantiate1 (pure ty) body)
    return $ TyArr ty resultType l

typeOf (Con s _ _) = gets (fst . (!s) . moduleCons)
typeOf (Int _ l) = return $ TyCon "Int" l
typeOf (Var ty _) = return ty
