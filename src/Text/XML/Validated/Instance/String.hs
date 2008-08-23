{-# LANGUAGE TypeSynonymInstances, PatternSignatures, ScopedTypeVariables, EmptyDataDecls,
    FunctionalDependencies, FlexibleInstances, FlexibleContexts,
    MultiParamTypeClasses, UndecidableInstances #-}
module Text.XML.Validated.Instance.String where
import Text.XML.Validated.Types
import Data.List

-- most simple string instance, XML is generated on directly
{-
instance (Show tagType) => CreateTag tagType String where
  createTag _ = "<" ++ (show (undefined :: tagType))

instance (Show attrType
         , AttributeType attrType
         ) => AddAttribute String attrType where 
  addAttribute tag _ value = 
      tag ++ " " ++ (show (undefined :: attrType) ) 
      ++ "=\"" ++ value ++ "\""
instance EndAttrs String String where
  endAttrs tag = tag ++ ">"
instance AddTag String where
  addTag = (++)
instance AddText String String where
  addText = (++)
instance EndTag String String where
  endTag = ( ++ ">")
-}
