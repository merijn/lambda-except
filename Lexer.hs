{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
module Lexer
    ( Parser
    , runParser
    , parseValid
    , varId
    , conId
    , varOp
    , conOp
    , lineSeparated
    , indented
    ) where

import Control.Applicative
import Control.Monad.State
import Data.Bifunctor
import Data.Char
import Data.Either.Validation
import Data.Int
import Text.Parser.LookAhead (LookAheadParsing, lookAhead)
import Text.Parser.Token.Highlight
import Text.Trifecta hiding (Parser, Failure, Success)
import Text.Trifecta.Delta (Delta, column)
import qualified Text.Trifecta as Trifecta

import Errors

data Layout = Indent Int64 | NewLayout deriving (Show)

newtype Parser a = Parser (StateT ([Error], [Layout]) Trifecta.Parser a)
  deriving
    ( Alternative, Applicative, CharParsing, DeltaParsing, Functor
    , LookAheadParsing, MarkParsing Delta, Monad, MonadPlus
    , MonadState ([Error], [Layout]), Parsing)

runParser :: Parser a -> Trifecta.Parser a
runParser (Parser p) = do
    (result, (errors, _)) <- runStateT p ([], [])
    if null errors
       then return result
       else raiseErr . failed $ "Errors: " ++ show errors

parseValid :: Monoid a => (e -> Errors) -> Validation e a -> Parser a
parseValid f p = case p of
    Failure n -> modify (first . mappend . f $ n) >> return mempty
    Success result -> return result

horizontalSpace :: Parser Int64
horizontalSpace = do
    start <- column <$> position
    many . satisfy $ \c -> isSpace c && c `notElem` "\n\r"
    end <- column <$> position
    return $ end - start

indent :: Parser ()
indent = try $ do
    i <- horizontalSpace
    gets snd >>= \case
        [] -> guard (i == 0)
        (Indent j:_) -> guard (i == j)
        (NewLayout:[]) -> guard (i > 0)
        (NewLayout:Indent j:_) -> guard (i > j)
        _ -> empty

lineComment :: Parser ()
lineComment = reserve varOp "--" >> void (manyTill anyChar newline)

multiLineComment :: Parser ()
multiLineComment = void $ do
    try (string "{-")
    manyTill (multiLineComment <|> void anyChar) (string "-}")

lineRemainder :: Parser ()
lineRemainder = try $ do
    horizontalSpace
    skipMany (multiLineComment >> horizontalSpace)
    skipOptional lineComment
    void $ lookAhead newline

blankLine :: Parser ()
blankLine = void $ lineRemainder >> newline

lineSeparated :: Parser a -> Parser [a]
lineSeparated p = p `sepEndBy` lineSep
  where
    lineSep :: Parser ()
    lineSep = try $ do
        void newline
        many blankLine
        indent

indented :: Parser a -> Parser a
indented p = do
    modify $ second (NewLayout:)
    result <- p
    modify $ second tail
    someSpace
    return result

instance Monoid a => Monoid (Parser a) where
    mempty = pure mempty
    mappend (Parser x) (Parser y) = Parser $ mappend <$> x <*> y

instance Errable Parser where
    raiseErr = Parser . lift . raiseErr

instance TokenParsing Parser where
    token p = do
        gets snd >>= \case
            (NewLayout:layouts) -> do
                i <- column <$> position
                modify . second $ const (Indent i:layouts)
            _ -> return ()

        p <* (someSpace <|> pure ())

    someSpace = do
        horizontalSpace
        skipOptional lineCont

      where
        lineCont = void . try $ do
            newline
            many blankLine
            i <- horizontalSpace
            gets snd >>= \case
                (Indent j:_)  -> guard (i > j)
                (NewLayout:_) -> empty
                _             -> guard (i > 0)

varId :: TokenParsing m => IdentifierStyle m
varId = IdentifierStyle
    { _styleName = "variable identifier"
    , _styleStart = lower
    , _styleLetter = alphaNum <|> oneOf "_'"
    , _styleReserved = [ "if", "then", "else", "let", "in", "case", "of"
                       , "_", "data", "where"]
    , _styleHighlight = Identifier
    , _styleReservedHighlight = ReservedIdentifier
    }

conId :: TokenParsing m => IdentifierStyle m
conId = IdentifierStyle
    { _styleName = "constructor identifier"
    , _styleStart = upper
    , _styleLetter = alphaNum <|> oneOf "_'"
    , _styleReserved = []
    , _styleHighlight = Constructor
    , _styleReservedHighlight = ReservedConstructor
    }

varOp :: TokenParsing m => IdentifierStyle m
varOp = IdentifierStyle
    { _styleName = "variable operator"
    , _styleStart = oneOf "!#$%&*+./<=>?@\\^|-~"
    , _styleLetter = _styleStart varOp <|> char ':'
    , _styleReserved = ["=", "\\", "->", "=>", "--", "@"]
    , _styleHighlight = Operator
    , _styleReservedHighlight = ReservedOperator
    }

conOp :: TokenParsing m => IdentifierStyle m
conOp = IdentifierStyle
    { _styleName = "constructor operator"
    , _styleStart = char ':'
    , _styleLetter = _styleStart conOp <|> oneOf "!#$%&*+./<=>?@\\^|-~"
    , _styleReserved = ["::"]
    , _styleHighlight = ConstructorOperator
    , _styleReservedHighlight = ReservedConstructorOperator
    }
