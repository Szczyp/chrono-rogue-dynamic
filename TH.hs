{-# LANGUAGE TemplateHaskell #-}

module TH (register) where

import Data.Char
import Data.Dynamic
import Data.Maybe
import Data.Monoid
import Language.Haskell.TH

declareFunction :: Name -> ExpQ -> TypeQ -> [DecQ]
declareFunction name expression signature =
    [ funD name [clause [] (normalB expression) []]
    , sigD name signature ]

componentInstance :: Name -> [DecQ]
componentInstance name =
    [ instanceD (cxt [])
        (appT (conT . mkName $ "Component") (conT name))
        [(funD (mkName "add") [clause [] (normalB
            [| \c e -> $setComponents (toDyn c : ($components . $(removeCmp name) $ e)) e |]) []])]]

entityType :: TypeQ
entityType = conT . mkName $ "Entity"

setComponents :: ExpQ
setComponents = global . mkName $ "setComponents"

components :: ExpQ
components = global . mkName $ "components"

getCmp :: ExpQ
getCmp = [| getFirst . mconcat . map (First . fromDynamic) . $components |]

getCmpOr :: ExpQ
getCmpOr = [| \c -> fromMaybe c . $getCmp |]

hasCmp :: Name -> ExpQ
hasCmp name = [| isJust . ($getCmp :: $entityType -> Maybe $(conT name)) |]

removeCmp :: Name -> ExpQ
removeCmp name = [| \e -> $setComponents ($filteredComponents e) e |]
    where filteredComponents = [| \e -> filter (isNothing . $convert) ($components e) |]
          convert = [| fromDynamic :: Dynamic -> Maybe $(conT name) |]

register :: Name -> DecsQ
register name = sequence . concat $
    [ declareFunction (prefix "get" name)
        getCmp
        [t| $entityType -> Maybe $(conT name) |]
    , declareFunction (prepostfix "get" "Or" name)
        getCmpOr
        [t| $(conT name) -> $entityType -> $(conT name) |]
    , declareFunction (prefix "has" name)
        (hasCmp name)
        [t| $entityType -> Bool |]
    , declareFunction (prefix "remove" name)
        (removeCmp name)
        [t| $entityType -> $entityType |]
    , componentInstance name ]
    where prefix p = mkName . (p ++) . nameBase
          postfix p = mkName . (++ p) . decapitalize
          prepostfix p p' = mkName . (p ++) . (++ p') . nameBase
          decapitalize = lower . nameBase
            where lower (c : cs) = toLower c : cs
