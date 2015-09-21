module Errors where

type Errors = [Error]

data Error
    = DupType String
    | DupDef String
    | DupCon String
    | DupBind String
    | MissType String
    | MissDef String
    deriving (Show)

dupType :: [String] -> Errors
dupType = map DupType

dupDef :: [String] -> Errors
dupDef = map DupDef

dupCon :: [String] -> Errors
dupCon = map DupCon

dupBind :: [String] -> Errors
dupBind = map DupBind

missType :: [String] -> Errors
missType = map MissType

missDef :: [String] -> Errors
missDef = map MissDef
