{-# LANGUAGE PatternSignatures, ScopedTypeVariables, EmptyDataDecls,
    FunctionalDependencies, FlexibleInstances, FlexibleContexts,
    MultiParamTypeClasses, UndecidableInstances #-}

module Text.XML.Validated.Types where
import Data.Either
{- unsafe abstract class to create xml documents
 - the typed validating stuff will use it to construct the xml document
 -}

data Content text tag =
  Text text
  | Tag tag


class VXML name tag attr attrValue text 
    |  attr -> tag, tag -> attr
      , tag -> name, tag -> text, tag -> attrValue where
  -- name value 
  attr :: name -> attrValue -> attr
  -- name attribute contents 
  tag :: name -> [ attr ] -> [ Content text tag ] -> tag

{- We encode the compile time XML validation state in type as well
 First a tag is created
 snd all attributes are added
 trd all subtags and text elements are added
 last it is added to another tag or the document

 These steps have been chosen because it's closest to String serialization
 which will be used most often
-}

-- small examples for testing only 

-- ========== classes to generate the result type ==================== 

class CreateTag tagType tag where
  createTag :: tagType -> tag

class (AttributeType attrType
  ) => AddAttribute tag attrType where 
  addAttribute :: tag -> attrType -> String -> tag -- Sting = attr value FIXME: extend to all attr types - still proof of concept

-- after this no attributes can be added 
class EndAttrs tag tag2 | tag -> tag2 where
  endAttrs :: tag -> tag2

class AddTag tag where
  addTag :: tag -> tag -> tag -- tag, child 
class AddText tag text where
  addText :: tag -> text -> tag -- tag, text node

-- after this no attributes can be added 
class EndTag tag tagFinal | tag -> tagFinal where
  endTag :: tag -> tagFinal


-- ========== typed classes ========================================== 
data PT a b = PT a b -- phontom type containing state a and the result b 

 -- you should never have to create an instance for AttributeType or TagType
 -- this is done automatically from DTD
class AttributeType attrType
class TagType tagType

class InitialState tagType state where
  initialState :: tagType -> state

class ( CreateTag tagType tag
      , InitialState tagType initialState
      ) => CreateTagT tagType initialState tag
      | tagType -> initialState where -- tag ready to append attributes 
  createTagT :: tagType -> PT initialState tag
  createTagT _  = PT (initialState (undefined :: tagType))
                     (createTag (undefined :: tagType))

class ( AttributeType attrType
      , AddAttribute tag attrType
      , StAddAttr attrType st st2
      ) => AddAttributeT attrType st st2 tag 
      | st -> st2 where 
  addAttributeT :: PT st tag -> attrType -> String -> PT st2 tag -- Sting = attr value FIXME: extend to all attr types - still proof of concept
  addAttributeT (PT _ t) _ v = PT (undefined :: st2)
                                  (addAttribute t (undefined :: attrType) v)
                     

-- after this no attributes can be added 
class ( EndAttrs tag tag2
      , StEndAttrs st st2
      ) => EndAttrsT tag tag2 st st2 where
  endAttrsT :: PT st tag -> PT st2 tag2
  endAttrsT (PT _ t) = PT (undefined :: st2) (endAttrs t)

class ( AddTag tag
      , StAddTag st stc st2
      ) => AddTagT tag st stc st2 where
  addTagT :: PT st tag -> PT stc tag -> PT st2 tag
  addTagT (PT _ t) (PT _ c) = PT (undefined :: st2) (addTag t c)
class ( AddText tag text
      , StAddText st st2
      ) => AddTextT tag tag2 text st st2 where
  addTextT :: PT st tag -> text -> PT st2 tag
  addTextT (PT _ t) text = PT (undefined :: st2) (addText t text)

-- after this no attributes can be added 
class ( EndTag tag tag2
      , StEndAttrs st st2
      ) => EndTagT tag tag2 st st2 where
  endTagT :: (PT st tag) -> (PT st2 tag2)

-- ========== the ugly part, the validation by transforming states === 

data Element attributes content

data ElementContent content modifier

-- content
data Element tagType
data Choice hlist
data Sequence hlist

-- modifier
data None  -- ^ Just One
data Query -- ^ Zero Or One
data Star  -- ^ Zero Or More
data Plus  -- ^ One Or More 


class (AttributeType attrType
      ) => StAddAttr attrType state state2 | attrType state -> state2
class StEndAttrs tag tag2 | tag -> tag2
class StAddTag tag child tag2 | tag child -> tag2
class StAddText tag tag2 | tag -> tag2
class StEndTag tag tag2 | tag -> tag2
