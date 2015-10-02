{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Parser (parseFromFile, parseFromData, moduleParser, expr) where

import Control.Applicative
import Control.Lens hiding (cons)
import Data.Bifunctor

import AST
import Errors
import Lexer
import UniqMap

parseUniq
    :: (Foldable f, Ord k)
    => ([k] -> Errors)
    -> f (k, v)
    -> Parser (UniqMap k v)
parseUniq f = parseValid f . buildUniqMap

data Declaration
    = Definition Name (Expr Name)
    | TypeAnn Name Type
    deriving (Eq,Ord,Show)

data TopLevelDeclaration
    = DataType DataType
    | Decl Declaration
    deriving (Eq,Ord,Show)

moduleParser :: Parser (Module Name)
moduleParser = do
    let topLevel = (Decl <$> declaration) <|> (DataType <$> dataParser)
    contents <- topLevel `sepEndBy` semi <* eof

    let rawTypes = [x | DataType x <- contents]

    types <- parseUniq dupData [(x^.dataName, x) | x <- rawTypes]
    cons <- parseUniq dupCon . concatMap addTypeToTypeCons $ rawTypes
    decls <- buildDecls [x | Decl x <- contents]
    return $ Module types cons decls
  where
    addTypeToTypeCons :: DataType -> [(Name, (Type, DataType))]
    addTypeToTypeCons t = ifoldMapOf (dataCons.ifolded) wrap t
      where wrap i x = [(i, (x, t))]

dataParser :: Parser DataType
dataParser = do
    reserve varId "data"
    typeName <- name conId
    reserve varId "where"
    constructors <- nesting $ constructor `sepEndBy` semi
    Data typeName <$> parseUniq dupCon constructors

constructor :: Parser (Name, Type)
constructor = (,) <$> name conId <* reserve conOp "::" <*> typeParser

buildDecls :: [Declaration] -> Parser (Decls Name)
buildDecls decls = do
    types <- parseUniq dupType [(n, t) | TypeAnn n t <- decls]
    defs <- parseUniq dupDef [(n, b) | Definition n b <- decls]

    let decMap | null types || null defs = pure mempty
               | otherwise = intersectUniq (,) defs types

    scopify <$> parseValid combineErrors decMap
  where
    combineErrors (ty, binds) = map MissType ty ++ map MissDef binds
    scopify d = first (abstractKeys d) <$> d

declaration :: Parser Declaration
declaration = do
    declName <- name varId
    definition declName <|> typeAnnotation declName
  where
    definition :: Name -> Parser Declaration
    definition n = Definition n <$ reserve varOp "=" <*> expr

    typeAnnotation :: Name -> Parser Declaration
    typeAnnotation n = TypeAnn n <$ reserve conOp "::" <*> typeParser

typeParser :: Parser Type
typeParser = arrType <|> primType
  where
    primType = parens typeParser <|> withPos (TyCon <$> ident conId)

    arrType = withPos $ do
        type1 <- try $ primType <* reserve varOp "->"
        type2 <- typeParser
        return $ TyArr type1 type2

expr :: Parser (Expr Name)
expr = chainl1 primExpr (pure app)
  where
    app e1 e2 = App e1 e2 $ Span begin end s
      where
        Span begin _ s = e1^?!spans
        Span _ end _ = e2^?!spans

    primExpr = choice
        [ parens expr
        , lambda
        , letBlock
        , caseOf
        , conExpr
        , intExpr
        , variable
        ]

lambda :: Parser (Expr Name)
lambda = withPos $ do
    reserve varOp "\\"
    (var, ty) <- parens binding <|> binding
    reserve varOp "->"
    body <- expr
    return $ Lambda var ty (abstract1 var body)
  where
    binding = (,) <$> name varId <*> bindType
    bindType = try (reserve varOp "::") *> (Just <$> typeParser)
           <|> pure Nothing

letBlock :: Parser (Expr Name)
letBlock = withPos $ do
    reserve varId "let"
    decls <- buildDecls =<< nesting (declaration `sepEndBy` semi)
    reserve varId "in"
    body <- expr
    return $ Let decls (abstractKeys decls body)

caseOf :: Parser (Expr Name)
caseOf = withPos $ do
    reserve varId "case"
    e <- expr
    reserve varId "of"
    alts <- nesting $ alternative `sepEndBy` semi
    return $ Case e alts

alternative :: Parser (Alt Expr Name)
alternative = withPos $ do
    pat <- pattern
    parseValid dupPatBind . buildUniqMap . fmap (,()) $ pat
    reserve varOp "->"
    body <- expr
    return $ Alt pat (abstractPos pat body)

pattern :: Parser (Pat Name)
pattern = parens pattern <|> choice (map withPos
    [ WildP <$ reserve varId "_"
    , AsP <$> try (name varId <* reserve varId "@") <*> pattern
    , VarP <$> name varId
    , ConP <$> name conId <*> many pattern
    ])

conExpr :: Parser (Expr Name)
conExpr = withPos $ Con <$> name conId <*> pure []

intExpr :: Parser (Expr Name)
intExpr = withPos $ Int <$> integer

variable :: Parser (Expr Name)
variable = withPos $ LocVar <$> name varId
