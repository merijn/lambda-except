{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module PrettyPrint
    ( prettyPrint
    , prettify
    , nested
    , prettyLoc
    , prettySource
    , prettyString
    ) where

import Prelude hiding ((<$>))
import Bound
import Data.List (intersperse)
import Text.PrettyPrint.ANSI.Leijen
import Text.Trifecta.Delta (Delta(..), nextTab)
import qualified Text.Trifecta.Rendering as Tri

import AST
import UniqMap

prettyPrint :: Prettify a => a -> IO ()
prettyPrint x = putDoc (prettify x) >> putStrLn ""

dblLine :: Doc
dblLine = line <> line

prettify :: Prettify a => a -> Doc
prettify = render False

nested :: Prettify a => a -> Doc
nested = render True

keyword :: String -> Doc
keyword = dullyellow . text

operator :: String -> Doc
operator = dullyellow . text

declKeyword :: String -> Doc
declKeyword = dullgreen . text

prettyLoc :: Loc -> Doc
prettyLoc (Loc (Span start end _)) =
    pretty start <+> text "-" <+> bold (pretty lineCount) <> text ":"
    <> bold (pretty columnCount)
  where
    lineCount, columnCount :: Int
    (lineCount, columnCount) = both %~ ((+1).fromIntegral) $ case end of
        Columns c _ -> (0, c)
        Tab x y _ -> (0, nextTab x + y)
        Lines l c _ _ -> (l, c)
        Directed _ l c _ _-> (l, c)

prettySource :: Loc -> Doc
prettySource (Loc l) = pretty . Tri.render $ l

prettyString :: Prettify a => a -> String
prettyString d = displayS (renderPretty 0.4 80 $ prettify d) ""

class Prettify a where
    render :: Bool -> a -> Doc
    default render :: Show a => Bool -> a -> Doc
    render _ = text . show

instance Prettify Name where
    render _ (Name s _) = text s

instance Prettify a => Prettify (Named a) where
    render b (Named _ x) = render b x

instance Prettify Type where
    render b (Type e) = render b e

instance Prettify b => Prettify (Var (Named a) b) where
    render _ (B (Named n _)) = prettify n
    render _ (F x) = prettify x

instance Prettify a => Prettify (Module a) where
    render _ m
        | null (m^.datas) && null (m^.decls) = empty
        | null (m^.datas) = prettify (m^.decls)
        | null (m^.decls) = prettyData (m^.datas)
        | otherwise = prettyData (m^.datas) <> dblLine
                   <> prettify (m^.decls)
      where
        prettyData = vsep . intersperse dblLine . map prettify . elems

instance Prettify a => Prettify (Data a) where
    render _ d = declKeyword "data"
             <+> prettify (d^.name)
             <+> operator ":"
             <+> prettify (d^.type_)
             <+> declKeyword "where"
             <$> indent 4 constructors
      where
        prettyCons (k, v) =
            prettify k <+> operator ":" <+> prettify v
        constructors = vsep . map prettyCons . assocs $ d^.cons

instance Prettify a => Prettify (Decls a) where
    render _ decs = mconcat . intersperse dblLine . map prettyDef $ assocs decs
      where
        prettyDef (decName, (def, ty)) =
          prettify decName <+> operator ":" <+> prettify (fromScope ty) <$>
          prettify decName <+> operator "=" <+>
          prettify (fromScope def)

instance Prettify a => Prettify (Alt Expr a) where
    render _ (Alt pat expr _) =
        prettify pat <+> operator "->" <+> prettify (fromScope expr)

instance Prettify a => Prettify (SimplePat a) where
    render _ WildP{} = text "_"
    render _ (VarP s _) = prettify s

instance Prettify a => Prettify (Pat a) where
    render _ (Simple p) = prettify p
    render _ (AsP s pat _) = prettify s <> operator "@" <> nested pat
    render isNested con@(ConP s pats _)
        | isNested = text "(" <> prettify con <> text ")"
        | otherwise = fillSep (prettify s : map prettify pats)

instance Prettify Const where
    render _ Star = text "*"
    render _ Box = text "□"

instance Prettify a => Prettify (Expr a) where
    render _ (Const c _) = prettify c
    render _ (Var x) = prettify x
    render _ (LocVar x _) = prettify x
    render _ (Con s [] _) = prettify s

    render isNested e@(Con s args _)
        | isNested = text "(" <> prettify e <> text ")"
        | otherwise = fillSep (prettify s : map prettify args)

    render isNested e@(Lambda argName ty body _)
        | isNested = text "(" <> prettify e <> text ")"
        | otherwise = operator "λ" <> binding <+> operator "->"
                  <+> prettify (fromScope body)
        where
          binding = text "(" <> prettify argName <+> operator ":"
                <+> prettify ty <> text ")"

    render isNested e@(Pi argName ty body _)
        | isNested = text "(" <> prettify e <> text ")"
        | WildP{} <- argName =
            prettify ty <+> operator "->" <+> prettify (fromScope body)
        | Const Star _ <- ty =
            operator "∀" <> prettify argName <+> operator "." <+> prettify (fromScope body)
        | otherwise =
            operator "Π" <> binding <+> operator "." <+> prettify (fromScope body)
        where
          binding = text "(" <> prettify argName <+> operator ":"
                <+> prettify ty <> text ")"

    render isNested e@(App e1 e2 _)
        | isNested = text "(" <> prettify e <> text ")"
        | otherwise = prettify e1 <+> nested e2

    render _ (Case e alts _) = keyword "case" <+> prettify e <+> keyword "of"
        <$> indent 4 (vsep (map prettify alts))

    render _ (Let decs body _) =
      keyword "let" <$> indent 4 (prettify decs)
      <$> keyword "in" <+> prettify (fromScope body)
