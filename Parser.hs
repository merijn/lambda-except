{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module Parser (parseFromFile, parseFromData, moduleParser, expr) where

import Prelude hiding (pi, span)
import Control.Applicative hiding (Const)
import Control.Arrow ((&&&))
import Data.Bifunctor
import Data.Monoid ((<>))

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
    | TypeAnn Name (Expr Name)
    deriving (Eq,Ord,Show)

data TopLevelDeclaration
    = DataType (Data Name)
    | Decl Declaration
    deriving (Eq,Ord,Show)

moduleParser :: Parser (Module Name)
moduleParser = do
    let topLevel = (Decl <$> declaration) <|> (DataType <$> dataParser)
    contents <- topLevel `sepEndBy` semi <* eof

    let rawTypes = [x | DataType x <- contents]

    types <- parseUniq dupData [(x^.name, x) | x <- rawTypes]
    constructors <- parseUniq dupCon . getCons $ rawTypes
    declarations <- buildDecls [x | Decl x <- contents]
    return $ Module types constructors declarations
  where
    getCons = foldMap $ splatCons <> view (cons.to assocs)
    splatCons :: Data a -> [(Name, Expr a)]
    splatCons = pure . (view name &&& view type_)

dataParser :: Parser (Data Name)
dataParser = do
    reserve varId "data"
    dataName <- nameId conId
    reserve conOp ":"
    kind <- expr
    reserve varId "where"
    constructors <- nesting $ dataConstructor `sepEndBy` semi
    Data dataName kind <$> parseUniq dupCon constructors

dataConstructor :: Parser (Name, Expr Name)
dataConstructor = (,) <$> nameId conId <* reserve conOp ":" <*> expr

buildDecls :: [Declaration] -> Parser (Decls Name)
buildDecls decs = do
    types <- parseUniq dupType [(n, t) | TypeAnn n t <- decs]
    defs <- parseUniq dupDef [(n, b) | Definition n b <- decs]

    let decMap | null types || null defs = pure mempty
               | otherwise = intersectUniq (,) defs types

    scopify <$> parseValid combineErrors decMap
  where
    combineErrors (ty, binds) = map MissType ty ++ map MissDef binds
    scopify d = bimap (abstractKeys d) (abstractKeys d) <$> d

declaration :: Parser Declaration
declaration = do
    declName <- nameId varId
    definition declName <|> typeAnnotation declName
  where
    definition :: Name -> Parser Declaration
    definition n = Definition n <$ reserve varOp "=" <*> expr

    typeAnnotation :: Name -> Parser Declaration
    typeAnnotation n = TypeAnn n <$ reserve conOp ":" <*> expr

expr :: Parser (Expr Name)
expr = choice
    [ lambda
    , pi
    , typeLambda
    , forall
    , arrow
    , letBlock
    , caseOf
    , appExpr
    ]

appExpr :: Parser (Expr Name)
appExpr = chainl1 primExpr (pure app)
  where
    app :: Expr Name -> Expr Name -> Expr Name
    app e1 e2 = App e1 e2 $ Loc (Span begin end s)
      where
        Span begin _ s = e1^.location.span
        Span _ end _ = e2^.location.span

lambda :: Parser (Expr Name)
lambda = withPos $ do
    reserve varOp "\\"
    (var, ty) <- typedBinding
    reserve varOp "->"
    body <- expr
    return $ Lambda var ty (abstractPat var body)

pi :: Parser (Expr Name)
pi = withPos $ do
    reserve varOp "|~|" <|> reserve conId "Π"
    (var, ty) <- typedBinding
    reserve varOp "."
    body <- expr
    return $ Pi var ty (abstractPat var body)

typeLambda :: Parser (Expr Name)
typeLambda = withPos $ do
    reserve varOp "/\\" <|> reserve conId "Λ"
    (var, ty) <- typedBinding
    reserve varOp "."
    body <- expr
    return $ Lambda var ty (abstractPat var body)

forall :: Parser (Expr Name)
forall = withPos $ do
    reserve varOp "\\/" <|> reserve varOp "∀" <|> reserve varId "forall"
    var <- simplePattern
    reserve varOp "."
    body <- expr
    return $ Pi var (Const Star (var^.location)) (abstractPat var body)

arrow :: Parser (Expr Name)
arrow = withPos $ do
    e1 <- try $ appExpr <* reserve varOp "->"
    e2 <- expr
    return $ \s -> let pat = WildP s in Pi pat e1 (abstractPat pat e2) s

letBlock :: Parser (Expr Name)
letBlock = withPos $ do
    reserve varId "let"
    decs <- buildDecls =<< nesting (declaration `sepEndBy` semi)
    reserve varId "in"
    body <- expr
    return $ Let decs (abstractKeys decs body)

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
pattern = parens pattern <|> choice
    [ withPos $ AsP <$> try (nameId varId <* reserve varId "@") <*> pattern
    , Simple <$> simplePattern
    , withPos $ ConP <$> nameId conId <*> many pattern
    ]

simplePattern :: Parser (SimplePat Name)
simplePattern = withPos $ choice
    [ VarP <$> nameId varId
    , WildP <$ reserve varId "_"
    ]

primExpr :: Parser (Expr Name)
primExpr = choice [variable, constructor, star, parens expr]

variable :: Parser (Expr Name)
variable = untyped
  where
    untyped :: Parser (Expr Name)
    untyped = withPos $ LocVar <$> nameId varId

typedBinding :: Parser (SimplePat Name, Expr Name)
typedBinding = parens tvar <|> tvar
  where
    tvar = do
        n <- try $ simplePattern <* reserve varOp ":"
        t <- expr
        return (n, t)

constructor :: Parser (Expr Name)
constructor = withPos $ Con <$> nameId conId <*> pure []

star :: Parser (Expr Name)
star = withPos $ Const Star <$ reserve varOp "*"
