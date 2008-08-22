module Text.XML.Validated.Util where
import Data.Char
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

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
