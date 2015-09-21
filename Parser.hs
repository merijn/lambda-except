{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Parser (runParser, moduleParser, expr) where

import Bound
import Control.Applicative
import Data.Bifunctor
import Text.Trifecta hiding (Parser, Result(..))

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

withPos :: Parser (Loc -> a) -> Parser a
withPos p = do
    start <- position
    result <- p
    end <- position
    return $ result (Loc start end)

data Declaration
    = Definition (String, Expr String)
    | TypeAnn (String, Type)

data TopLevelDeclaration
    = DataType DataType
    | Decl Declaration

moduleParser :: Parser (Module String)
moduleParser = do
    contents <- lineSeparated $ DataType <$> dataParser
                            <|> Decl <$> declaration

    let rawTypes = [x | DataType x <- contents]

    types <- parseUniq dupType [(dataName x, x) | x <- rawTypes]
    cons <- parseUniq dupCon . concatMap addTypeToTypeCons $ rawTypes
    decls <- buildDecls [x | Decl x <- contents]
    return $ Module types cons decls
  where
    addTypeToTypeCons t = map (second (,t)) . assocs $ dataCons t

dataParser :: Parser DataType
dataParser = do
    reserve varId "data"
    name <- ident conId
    reserve varId "where"
    constructors <- indented $ lineSeparated constructor
    Data name <$> parseUniq dupCon constructors

constructor :: Parser (String, Type)
constructor = (,) <$> ident conId <* reserve conOp "::" <*> typeParser

buildDecls :: [Declaration] -> Parser (Decls String)
buildDecls decls = do
    types <- parseUniq dupType [t | TypeAnn t <- decls]
    defs <- parseUniq dupDef [d | Definition d <- decls]

    let decMap | null types || null defs = pure mempty
               | otherwise = intersectUniq (,) defs types

    scopify <$> parseValid combineErrors decMap
  where
    combineErrors (ty, binds) = map MissType ty ++ map MissDef binds
    scopify d = first (abstractKeys d) <$> d

declaration :: Parser Declaration
declaration = Definition <$> definition <|> TypeAnn <$> typeAnnotation

definition :: Parser (String, Expr String)
definition = do
    name <- try $ ident varId <* reserve varOp "="
    expression <- expr
    return (name, expression)

typeAnnotation :: Parser (String, Type)
typeAnnotation = do
    name <- try $ ident varId <* reserve conOp "::"
    typeAnn <- typeParser
    return (name, typeAnn)

typeParser :: Parser Type
typeParser = arrType <|> primType
  where
    primType = parens typeParser <|> withPos (TyCon <$> ident conId)

    arrType = withPos $ do
        type1 <- try $ primType <* reserve varOp "->"
        type2 <- typeParser
        return $ TyArr type1 type2

expr :: Parser (Expr String)
expr = appExpr <|> primExpr
  where
    primExpr = choice
        [ parens expr
        , lambda
        , letBlock
        , caseOf
        , conExpr
        , intExpr
        , ifExpr
        , variable
        ]

    appExpr = withPos . try $ do
        expr1 <- primExpr
        expr2 <- expr
        return $ App expr1 expr2

lambda :: Parser (Expr String)
lambda = withPos $ do
    reserve varOp "\\"
    (var, ty) <- parens binding <|> binding
    reserve varOp "->"
    body <- expr
    return $ Lambda var ty (abstract1 var body)
  where
    binding = (,) <$> ident varId <* reserve varOp "::" <*> typeParser

letBlock :: Parser (Expr String)
letBlock = withPos $ do
    reserve varId "let"
    decls <- buildDecls =<< indented (lineSeparated declaration)
    reserve varId "in"
    body <- expr
    return $ Let decls (abstractKeys decls body)

caseOf :: Parser (Expr String)
caseOf = withPos $ do
    reserve varId "case"
    e <- expr
    reserve varId "of"
    alts <- indented $ lineSeparated alternative
    return $ Case e alts

alternative :: Parser (Alt Expr String)
alternative = withPos $ do
    pat <- pattern
    parseValid dupBind . buildUniqMap . fmap (,()) $ pat
    reserve varOp "->"
    body <- expr
    return $ Alt pat (abstractPos pat body)

pattern :: Parser (Pat String)
pattern = parens pattern <|> choice (map withPos
    [ WildP <$ reserve varId "_"
    , AsP <$> try (ident varId <* reserve varId "@") <*> pattern
    , VarP <$> ident varId
    , ConP <$> ident conId <*> pure []
    ])

conExpr :: Parser (Expr String)
conExpr = withPos $ Con <$> ident conId <*> many expr

intExpr :: Parser (Expr String)
intExpr = withPos $ Int <$> integer

variable :: Parser (Expr String)
variable = withPos $ Var <$> ident varId

ifExpr :: Parser (Expr String)
ifExpr = withPos $ do
    reserve varId "if"
    b <- expr
    reserve varId "then"
    (e1, pos1) <- withPos $ fmap (,) expr
    reserve varId "else"
    (e2, pos2) <- withPos $ fmap (,) expr
    return $ Case b [ Alt (ConP "True" [] Builtin) (scope e1) pos1
                    , Alt (ConP "False" [] Builtin) (scope e2) pos2
                    ]
  where
    scope = abstract $ const Nothing
