{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module PrettyPrint where

import Bound
import Data.Foldable

import AST
import UniqMap

prettyPrint :: Prettify a => a -> IO ()
prettyPrint = putStr . prettify 0

class Prettify a where
    prettify :: Int -> a -> String
    default prettify :: Show a => Int -> a -> String
    prettify _ = show

instance Prettify (Module String) where
    prettify i m = unlines (map (prettify i) (elems (moduleTypes m)))
        ++ prettify i (moduleDecls m)

instance Prettify DataType where
    prettify i d = "data " ++ dataName d ++ " where\n"
        ++ unlines (map printCons (assocs (dataCons d)))
      where
        printCons (k, v) = indent ++ k ++ " :: " ++ prettify i v
        indent = replicate (i + 4) ' '

instance Prettify Type where
    prettify _ TyAny = "a"
    prettify _ (TyCon s _) = s
    prettify i (TyArr t1 t2 _) =
      "(" ++ prettify i t1 ++ " -> " ++ prettify i t2 ++ ")"

instance Prettify (Decls String) where
    prettify i decls = drop 1 . foldr prettyDef "" $ assocs decls
      where
        prettyDef (name, (def, ty)) rest =
          "\n" ++ replicate i ' ' ++ name ++ " :: " ++ prettify i ty
          ++ "\n" ++ replicate i ' ' ++ name ++ " = "
          ++ prettify i (instantiate (pure . nameAt decls) def) ++ "\n" ++ rest

instance Prettify (Alt Expr String) where
    prettify i (Alt pat expr _) = replicate i ' ' ++ prettify i pat
        ++ " -> " ++ prettify i (instantiate getName expr)
      where
        getName = pure . (toList pat !!)

instance Prettify (Pat String) where
    prettify _ (VarP s _) = s
    prettify _ WildP{} = "_"
    prettify i (AsP s pat _) = s ++ "@" ++ prettify i pat
    prettify i (ConP s pats _) = "(" ++ s ++ " "
        ++ unwords (map (prettify i) pats) ++ ")"

instance Prettify (Expr String) where
    prettify i (App e1 e2 _) =
      "(" ++ prettify i e1 ++ ") (" ++ prettify i e2 ++ ")"
    prettify i (Lambda name t body _) =
      "\\(" ++ name ++ " :: " ++ prettify i t ++ ") => " ++ prettify i (instantiate1 (return name) body)
    prettify i (Let decls body _) =
      "let\n" ++ prettify (i + 4) decls ++ replicate i ' ' ++ "in "
      ++ prettify i (instantiate (pure . nameAt decls) body)
    prettify i (Con s args _) =
      "(" ++ s ++ unwords (map (prettify i) args) ++ ")"
    prettify i (Case e alts _) = "case " ++ prettify i e ++ " of\n" ++
        unlines (map (prettify (i + 4)) alts)
    prettify _ (Int i _) = show i
    prettify _ (Var x _) = x
