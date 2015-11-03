module Errors where

import Prelude hiding ((<$>), span)
import Control.Lens
import Data.List (group, sort)
import Text.PrettyPrint.ANSI.Leijen hiding (group)

import AST
import PrettyPrint

type Errors = [Error]

dblLine :: Doc
dblLine = line <> line

data Error
    = DupType Name
    | DupData Name
    | DupDef Name
    | DupCon Name
    | DupPatBind Name
    | MissType Name
    | MissDef Name
    | UnboundTypeVar Name
    deriving (Show)

dupType :: [Name] -> Errors
dupType = map DupType

dupData :: [Name] -> Errors
dupData = map DupData

dupDef :: [Name] -> Errors
dupDef = map DupDef

dupCon :: [Name] -> Errors
dupCon = map DupCon

dupPatBind :: [Name] -> Errors
dupPatBind = map DupPatBind

missType :: [Name] -> Errors
missType = map MissType

missDef :: [Name] -> Errors
missDef = map MissDef

unboundTypeVar :: [Name] -> Errors
unboundTypeVar = map UnboundTypeVar

reportDuplicates :: String -> [Name] -> Doc
reportDuplicates desc = vsep . map reportDuplicate . group
  where
    printLoc :: Loc -> Doc
    printLoc s = prettyLoc s <$> prettySource s

    reportDuplicate :: [Name] -> Doc
    reportDuplicate [] = error "Can't happen"
    reportDuplicate ns@(n:_) = vsep
        [ hsep [ text "Duplicate"
               , string desc
               , text "for"
               , bold (prettify n) <> text ":"
               ]
        , indent 4 . vsep . map printLoc . sort $ ns^..folded.location
        ]

reportMissing :: String -> Name -> Doc
reportMissing desc n = vsep
  [ hsep [ text "Missing"
         , string desc
         , text "for"
         , bold (prettify n) <> text ":"
         ]
  , prettyLoc (n^.location)
  , prettySource (n^.location)
  ]

reportErrors :: Errors -> Doc
reportErrors es = goDup
    [ ("type annotations", [n | DupType n <- es])
    , ("data declarations", [n | DupData n <- es])
    , ("definitions", [n | DupDef n <- es])
    , ("constructor names", [n | DupCon n <- es])
    , ("pattern binds", [n | DupPatBind n <- es])
    ]
    <> goMiss
    [ ("type annotation", [n | MissType n <- es])
    , ("definition", [n | MissDef n <- es])
    ]
    <> vsep (map reportUnbound [n | UnboundTypeVar n <- es])
  where
    goDup :: [(String, [Name])] -> Doc
    goDup [] = empty
    goDup ((desc, names):l)
        | null names = goDup l
        | null l = errs
        | otherwise = errs <> dblLine <> goDup l
        where
          errs :: Doc
          errs = reportDuplicates desc names

    goMiss :: [(String, [Name])] -> Doc
    goMiss [] = empty
    goMiss ((desc, names):l)
        | null names = goMiss l
        | null l = errs
        | otherwise = errs <$> goMiss l
        where
          errs :: Doc
          errs = vsep $ map (reportMissing desc) names

    reportUnbound :: Name -> Doc
    reportUnbound n = vsep
        [ hsep [ string "Unbound type variable"
               , bold (prettify n) <> text ":"
               ]
        , prettyLoc (n^.location)
        , prettySource (n^.location)
        ]
