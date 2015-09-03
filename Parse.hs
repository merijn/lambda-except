module Parse (runParser, declarations, expr) where

import Bound
import Control.Applicative
import Data.Functor
import Text.Trifecta hiding (Parser)

import AST
import Lexer

withPos :: Parser (Loc -> a) -> Parser a
withPos p = do
    start <- position
    result <- p
    end <- position
    return $ result (Loc start end)

declarations :: Parser (Decls String)
declarations = buildDecls <$> declaration `sepEndBy` lineSep

declaration :: Parser (String, Expr String)
declaration = do
    name <- ident varId
    reserve varOp "="
    expression <- expr
    return (name, expression)

typeParser :: Parser Type
typeParser = arrType <|> primType
  where
    boolType = withPos $ TyBool <$ reserve conId "Bool"
    intType = withPos $ TyInt <$ reserve conId "Int"
    primType = parens typeParser <|> boolType <|> intType

    arrType = withPos $ do
        type1 <- try $ primType <* reserve varOp "->"
        type2 <- typeParser
        return $ TyArr type1 type2

expr :: Parser (Expr String)
expr = appExpr <|> primExpr
  where
    primExpr = parens expr <|> lambda <|> letBlock <|> boolExpr <|> intExpr
                <|> ifExpr <|> variable

    appExpr = withPos . try $ do
        expr1 <- primExpr
        expr2 <- expr
        return $ App expr1 expr2

lambda :: Parser (Expr String)
lambda = withPos $ do
    reserve varOp "\\"
    (var, ty) <- parens pattern <|> pattern
    reserve varOp "=>"
    body <- expr
    return $ Lambda var ty (abstract1 var body)
  where
    pattern = (,) <$> ident varId <* reserve varOp "::" <*> typeParser

letBlock :: Parser (Expr String)
letBlock = withPos $ do
    reserve varId "let"
    decls <- indented declarations <* someSpace
    reserve varId "in"
    body <- expr
    return $ buildLet decls body

boolExpr :: Parser (Expr String)
boolExpr = withPos $ do
    val <- trueParser <|> falseParser
    return $ Bool val
  where
    trueParser = reserve conId "True" $> True
    falseParser = reserve conId "False" $> False

intExpr :: Parser (Expr String)
intExpr = withPos $ Int <$> integer

variable :: Parser (Expr String)
variable = withPos $ Var <$> ident varId

ifExpr :: Parser (Expr String)
ifExpr = withPos $ do
    reserve varId "if"
    b <- expr
    reserve varId "then"
    e1 <- expr
    reserve varId "else"
    e2 <- expr
    return $ If b e1 e2
