{-# LANGUAGE TypeSynonymInstances, PatternSignatures, ScopedTypeVariables, EmptyDataDecls,
    FunctionalDependencies, FlexibleInstances, FlexibleContexts,
    MultiParamTypeClasses, UndecidableInstances #-}
module Text.XML.Validated.Instance.String where
import Data.Char
import Text.XML.Validated.Types
import Data.List

-- most simple string instance, XML is generated on directly
instance (Show elType) => CreateEl elType String where
  createEl _ = "<" ++ (show (undefined :: elType))

instance (Show attrType
         ) => AddAttribute String attrType where 
  addAttribute tag _ value = 
      tag ++ " " ++ (show (undefined :: attrType) ) 
      ++ "=\"" ++ (stringToHtmlString value) ++ "\""
instance EndAttrs String String where
  endAttrs tag = tag ++ ">"
instance AddEl String String where
  addEl = (++)
instance AddText String String where
  addText el text = el ++ (stringToHtmlString text)
instance (Show elType) => EndEl elType String String where
  endEl _ = (++ ("</" ++ (show (undefined :: elType)) ++ ">"))
instance EndAttrsEndElement elType String String where
  endAttrsEndElementDeclaredEmpty _ s = s ++ "/>"
  -- endAttrsEndElement _ s = endEl (undefined :: elType) . endAttrs

  -- empty attributes should only be used for elements declared as EMPTY for interoperability
  -- I use them for all empty tags to have less output, so if you have any
  -- trouble uncomment the second line above

instance ( Show rootElType
        ) => XmlDoc rootElType String String where
  xmlDoc _ (XmlIds pub sys) xml = 
            "<!DOCTYPE " 
              ++ (show (undefined :: rootElType)) 
              ++ (maybe "" (\pub -> " PUBLIC \"" ++ pub ++ "\" ") pub ) 
              ++ (maybe "" (\sys -> "\"" ++ sys ++"\"") sys )
              ++ ">\n"
            ++ xml

-- taken from xhtml, FIXME check that it's sufficient
-- | Processing Strings into Html friendly things.
stringToHtmlString :: String -> String
stringToHtmlString = concatMap fixChar
    where
      fixChar '<' = "&lt;"
      fixChar '>' = "&gt;"
      fixChar '&' = "&amp;"
      fixChar '"' = "&quot;"
      fixChar c | ord c < 0xff = [c]
      fixChar c = "&#" ++ show (ord c) ++ ";"
