{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
module PrettyPrint where

import Bound
import AST

getDeclName :: Applicative f => Decl f a -> Int -> f String
getDeclName (Decls decls) = pure . (map fst decls !!)

unScope :: Decls String -> Scope Int Expr String -> Expr String
unScope = instantiate . getDeclName

prettyPrint :: Prettify a => a -> IO ()
prettyPrint = putStr . prettify 0

class Prettify a where
    prettify :: Int -> a -> String
    default prettify :: Show a => Int -> a -> String
    prettify _ = show

instance Prettify Type where
    prettify _ (TyBool _) = "Bool"
    prettify _ (TyInt _) = "Int"
    prettify i (TyArr t1 t2 _) =
      "(" ++ prettify i t1 ++ " -> " ++ prettify i t2 ++ ")"

instance Prettify (Decls String) where
    prettify _ (Decls []) = ""
    prettify i decls@(Decls defs) = drop 1 $ foldr prettyDef "" defs
      where
        prettyDef (name, def) rest =
          "\n" ++ replicate i ' ' ++ name ++ " = "
          ++ prettify i (unScope decls def) ++ "\n" ++ rest

instance Prettify (Expr String) where
    prettify i (App e1 e2 _) =
      "(" ++ prettify i e1 ++ ") (" ++ prettify i e2 ++ ")"
    prettify i (Lambda name t body _) =
      "\\(" ++ name ++ " :: " ++ prettify i t ++ ") => " ++ prettify i (instantiate1 (return name) body)
    prettify i (Let decls body _) =
      "let\n" ++ prettify (i + 4) decls ++ replicate i ' ' ++ "in "
      ++ prettify i (unScope decls body)
    prettify _ (Int i _) = show i
    prettify _ (Bool b _) = show b
    prettify _ (Var x _) = x
    prettify i (If p x y _) =
        "if " ++ prettify i p ++ " then " ++ prettify i x ++ " else " ++ prettify i y ++ ")"
