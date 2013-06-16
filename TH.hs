{-# LANGUAGE TemplateHaskell #-}

module TH where

import Prelude hiding (id)

import Data.Char
import Data.Dynamic
import Data.Maybe

import Language.Haskell.TH

declareFunction :: Name -> ExpQ -> TypeQ -> [DecQ]
declareFunction name expression signature =
    [ funD name [clause [] (normalB expression) []]
    , sigD name signature ]

makeComponent :: Name -> DecsQ
makeComponent name = sequence . concat $
    [ declareFunction component
        [| $getCmp |]
        [t| [Dynamic] -> Maybe $componentType |]
    , declareFunction componentOr
        [| $getCmpOr |]
        [t| $componentType -> [Dynamic] -> $componentType |]
    , declareFunction hasComponent
        [| isJust . ($getCmp :: [Dynamic] -> Maybe $componentType) |]
        [t| [Dynamic] -> Bool |]
    ]
    where component = mkName . decapitalize $ name
          componentOr = mkName . (++ "Or") . decapitalize $ name
          hasComponent = mkName . ("has" ++) . nameBase $ name
          componentType = conT name
          getCmp = globalName "getCmp"
          getCmpOr = globalName "getCmpOr"
          globalName = global . mkName
          decapitalize = lower . nameBase
            where lower (c : cs) = toLower c : cs
