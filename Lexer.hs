{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
module Lexer
    ( Parser
    , parseFromFile
    , parseFromData
    , parseValid
    , nameId
    , withPos
    , varId
    , conId
    , varOp
    , conOp
    , module Text.Trifecta
    ) where

import Control.Applicative
import Control.Monad.State
import Data.Bifunctor
import qualified Data.ByteString as Strict hiding (empty, snoc)
import Data.ByteString.UTF8 as UTF8
import Data.Char
import Data.Either.Validation
import Data.Semigroup.Reducer
import Text.Parser.LookAhead (LookAheadParsing, lookAhead)
import Text.Parser.Token.Highlight
import Text.Trifecta hiding (Parser, Failure, Success, parseFromFile)
import Text.Trifecta.Delta (Delta(..), column)
import qualified Text.Trifecta as Tri

import AST
import Errors

newtype Parser a = Parser (StateT ([Error], [Int]) Tri.Parser a)
  deriving
    ( Alternative, Applicative, CharParsing, DeltaParsing, Functor
    , LookAheadParsing, MarkParsing Delta, Monad, MonadPlus
    , MonadState ([Error], [Int]), Parsing)

runParser :: Reducer t Rope => Delta -> Parser a -> t -> Result a
runParser d (Parser p) inp = do
    case parse (runStateT p ([], [])) of
        Tri.Failure e -> Tri.Failure e
        Tri.Success (x, ([], _)) -> Tri.Success x
        Tri.Success (_, (errs, _)) -> Tri.Failure (reportErrors errs)
  where
    parse pa = starve $ feed inp $ stepParser (release d *> pa) mempty mempty

parseFromFile :: Parser a -> String -> IO (Result a)
parseFromFile p fn = do
  s <- Strict.readFile fn
  return $ runParser (Directed (UTF8.fromString fn) 0 0 0 0) p s

parseFromData :: Reducer t Rope => Parser a -> t -> Result a
parseFromData = runParser (Lines 0 0 0 0)

parseValid :: Monoid a => (e -> Errors) -> Validation e a -> Parser a
parseValid f p = case p of
    Failure n -> modify (first . mappend . f $ n) >> return mempty
    Success result -> return result

nameId :: IdentifierStyle (Unspaced Parser) -> Parser Name
nameId style = do
    (result :~ s) <- spanned . runUnspaced $ ident style
    someSpace
    return $ Name result (Loc s)

withPos :: DeltaParsing m => m (Loc -> a) -> m a
withPos p = do
    (result :~ pos) <- spanned p
    return $ result (Loc pos)

horizontalSpace :: Parser Int
horizontalSpace = do
    start <- column <$> position
    many . satisfy $ \c -> isSpace c && c `notElem` "\n\r"
    end <- column <$> position
    return . fromIntegral $ end - start

indent :: Parser ()
indent = try $ do
    i <- horizontalSpace
    gets snd >>= \case
        (j:_) -> guard (i == j)
        _     -> guard (i == 0)

lineComment :: Parser ()
lineComment = highlight Comment $
    reserve varOp "--" >> void (manyTill anyChar newline)

multiLineComment :: Parser ()
multiLineComment = void . highlight Comment $ do
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

instance Monoid a => Monoid (Parser a) where
    mempty = pure mempty
    mappend (Parser x) (Parser y) = Parser $ mappend <$> x <*> y

instance Errable Parser where
    raiseErr = Parser . lift . raiseErr

instance TokenParsing Parser where
    semi = try $ do
        void newline
        many blankLine
        indent
        return ';'

    nesting p = do
        i <- fromIntegral . column <$> position
        gets snd >>= \case
            (j:_) -> guard (i > j)
            _     -> guard (i > 0)
        modify $ second (i:)
        result <- p
        modify $ second tail
        someSpace
        return result

    someSpace = do
        horizontalSpace
        skipOptional . try $ do
            newline
            many blankLine
            i <- horizontalSpace
            gets snd >>= \case
                (j:_) -> guard (i > j)
                _     -> guard (i > 0)

varId :: TokenParsing m => IdentifierStyle m
varId = IdentifierStyle
    { _styleName = "variable identifier"
    , _styleStart = lower
    , _styleLetter = alphaNum <|> oneOf "_'"
    , _styleReserved = [ "let", "in", "case", "of" , "_", "data", "where"
                       , "forall"]
    , _styleHighlight = Identifier
    , _styleReservedHighlight = ReservedIdentifier
    }

conId :: TokenParsing m => IdentifierStyle m
conId = IdentifierStyle
    { _styleName = "constructor identifier"
    , _styleStart = upper
    , _styleLetter = alphaNum <|> oneOf "_'"
    , _styleReserved = ["Λ", "Π"]
    , _styleHighlight = Constructor
    , _styleReservedHighlight = ReservedConstructor
    }

varOp :: TokenParsing m => IdentifierStyle m
varOp = IdentifierStyle
    { _styleName = "variable operator"
    , _styleStart = oneOf "!#$%&*+./<=>?@\\^|-~" <|> satisfy isSymbol
    , _styleLetter = _styleStart varOp <|> char ':'
    , _styleReserved = [ "=", "\\", "->", "--", "@", "*", "|~|", "/\\"
                       , "\\/", "∀", "."]
    , _styleHighlight = Operator
    , _styleReservedHighlight = ReservedOperator
    }

conOp :: TokenParsing m => IdentifierStyle m
conOp = IdentifierStyle
    { _styleName = "constructor operator"
    , _styleStart = char ':'
    , _styleLetter = _styleStart conOp <|> _styleStart varOp
    , _styleReserved = [":"]
    , _styleHighlight = ConstructorOperator
    , _styleReservedHighlight = ReservedConstructorOperator
    }
