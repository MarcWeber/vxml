{-# LANGUAGE TemplateHaskell #-}
module Text.XML.Validated.Util where
import Language.Haskell.TH
import Data.Char
import Data.HList
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import Text.XML.Validated.Types

fstUpper (x:xs) = toUpper x : xs
fstLower (x:xs) = toLower x : xs

cName = conT . mkName
-- vName = varE . mkName

appEn :: ExpQ -> [ ExpQ ] -> ExpQ
appEn t l = appn appE t l
appTn t l = appn appT t l
appn app f = appn' f . reverse
  where appn' f [] = f
        appn' f (a:as) = app (appn' f as) a

-- undefined :: t
undType :: TypeQ -> ExpQ
undType t = sigE (varE 'undefined) t

-- contstruct HList 
hlist [] = conT ''HNil
hlist (x:xs) = appTn (conT ''HCons) [x, hlist xs]
-- contstruct Seq list
list r m [x] = error $ m ++ " list with only on element! " ++ ( pprint $ unsafePerformIO $ runQ x)
list t m [x,y] = appTn t [x, y]
list t m (x:xs) = appTn t [x, list t m xs]
seqList, choiceList :: [ TypeQ ] -> TypeQ
seqList = list (conT ''Seq) "sequence"
choiceList = list (conT ''Or) "choice"
