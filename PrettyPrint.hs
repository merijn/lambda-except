{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module PrettyPrint
    ( prettyPrint
    , prettify
    , nested
    , prettySpan
    , prettySource
    ) where

import Prelude hiding ((<$>))
import Bound
import Control.Lens
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

typeColour :: String -> Doc
typeColour = blue . text

prettySpan :: Span -> Doc
prettySpan (Span start end _) =
    pretty start <+> text "-" <+> bold (pretty lineCount) <> text ":"
    <> bold (pretty columnCount)
  where
    lineCount, columnCount :: Int
    (lineCount, columnCount) = both %~ ((+1).fromIntegral) $ case end of
        Columns c _ -> (0, c)
        Tab x y _ -> (0, nextTab x + y)
        Lines l c _ _ -> (l, c)
        Directed _ l c _ _-> (l, c)

prettySource :: Span -> Doc
prettySource = pretty . Tri.render

class Prettify a where
    render :: Bool -> a -> Doc
    default render :: Show a => Bool -> a -> Doc
    render _ = text . show

instance Prettify Name where
    render _ (Name s _) = text s

instance Prettify (Named Type) where
    render _ (Named n ty) =
      text "(" <> prettify n <+> operator "::" <+> prettify ty <> text ")"

instance Prettify (Var (Named a) b) where
    render _ (B (Named n _)) = prettify n
    render _ (F _) = text "?"

instance Prettify (Module a) where
    render _ m
        | null (m^.modTypes) = prettify (m^.modDecls)
        | null (m^.modDecls) = prettyData (m^.modTypes)
        | otherwise = prettyData (m^.modTypes) <> dblLine
                   <> prettify (m^.modDecls)
      where
        prettyData = vsep . intersperse dblLine . map prettify . elems

instance Prettify DataType where
    render _ d = declKeyword "data"
             <+> prettify (d^.dataName)
             <+> declKeyword "where"
             <$> indent 4 constructors
      where
        prettyCons (k, v) =
            prettify k <+> operator "::" <+> prettify v
        constructors = vsep . map prettyCons . assocs $ d^.dataCons

instance Prettify Type where
    render _ TyAny{} = typeColour "?"
    render _ (TyCon s _) = typeColour s
    render isNested ty@(TyArr t1 t2 _)
        | isNested = text "(" <> prettify ty <> text ")"
        | otherwise = nested t1 <+> operator "->" <+> prettify t2

instance Prettify (Decls a) where
    render _ decls = mconcat . intersperse dblLine . map prettyDef $ assocs decls
      where
        prettyDef (name, (def, ty)) =
          prettify name <+> operator "::" <+> prettify ty <$>
          prettify name <+> operator "=" <+>
          prettify (fromScope def)

instance Prettify (Alt Expr a) where
    render _ (Alt pat expr _) =
        prettify pat <+> operator "->" <+> prettify (fromScope expr)

instance Prettify a => Prettify (Pat a) where
    render _ (VarP s _) = prettify s
    render _ WildP{} = text "_"
    render _ (AsP s pat _) = prettify s <> operator "@" <> nested pat
    render isNested con@(ConP s pats _)
        | isNested = text "(" <> prettify con <> text ")"
        | otherwise = fillSep (prettify s : map prettify pats)

instance Prettify a => Prettify (Expr a) where
    render isNested e@(App e1 e2 _)
        | isNested = text "(" <> prettify e <> text ")"
        | otherwise = prettify e1 <+> nested e2

    render isNested e@(Lambda name ty body _)
        | isNested = text "(" <> prettify e <> text ")"
        | otherwise = operator "\\" <> binding <+> operator "->"
                  <+> prettify (fromScope body)
        where
          binding
            | Just t <- ty = text "(" <> prettify name
                         <+> operator "::" <+> prettify t
                          <> text ")"
            | otherwise = prettify name

    render _ (Let decls body _) =
      keyword "let" <$> indent 4 (prettify decls)
      <$> keyword "in" <+> prettify (fromScope body)

    render _ (Con s [] _) = prettify s

    render isNested e@(Con s args _)
        | isNested = text "(" <> prettify e <> text ")"
        | otherwise = fillSep (prettify s : map prettify args)

    render _ (Case e alts _) = keyword "case" <+> prettify e <+> keyword "of"
        <$> indent 4 (vsep (map prettify alts))
    render _ (Int i _) = integer i
    render _ (LocVar x _) = prettify x
    render _ (Var x) = prettify x
