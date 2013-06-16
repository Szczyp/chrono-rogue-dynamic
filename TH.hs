{-# LANGUAGE TemplateHaskell #-}

module TH where

import Prelude hiding (id)

import Data.Char
import Data.Dynamic

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
    ]
    where component = mkName . decapitalize $ name
          componentOr = mkName $ decapitalize name ++ "Or"
          getCmp = globalName "getCmp"
          getCmpOr = globalName "getCmpOr"
          componentType = conT name
          globalName = global . mkName
          decapitalize = lower . nameBase
            where lower (c : cs) = toLower c : cs
