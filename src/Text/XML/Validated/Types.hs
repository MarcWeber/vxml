{-# LANGUAGE PatternSignatures, ScopedTypeVariables, EmptyDataDecls,
    FunctionalDependencies, FlexibleInstances, FlexibleContexts,
    MultiParamTypeClasses, UndecidableInstances #-}

module Text.XML.Validated.Types where
import Data.HList
import Data.Either
{- unsafe abstract class to create xml documents
 - the typed validating stuff will use it to construct the xml document
 -}

data Content text el =
  Text text
  | El el


class VXML name el attr attrValue text 
    |  attr -> el, el -> attr
      , el -> name, el -> text, el -> attrValue where
  -- name value 
  attr :: name -> attrValue -> attr
  -- name attribute contents 
  el :: name -> [ attr ] -> [ Content text el ] -> el

{- We encode the compile time XML validation state in type as well
 First a el is created
 snd all attributes are added
 trd all subels and text elements are added
 last it is added to another el or the document

 These steps have been chosen because it's closest to String serialization
 which will be used most often
-}

-- small examples for testing only 

-- ========== classes to generate the result type ==================== 

class CreateEl elType el where
  createEl :: elType -> el

class (AttributeType attrType
  ) => AddAttribute el attrType where 
  addAttribute :: el -> attrType -> String -> el -- Sting = attr value FIXME: extend to all attr types - still proof of concept

-- after this no attributes can be added 
class EndAttrs el el2 | el -> el2 where
  endAttrs :: el -> el2

class AddEl el where
  addEl :: el -> el -> el -- el, child 
class AddText el text where
  addText :: el -> text -> el -- el, text node

-- after this no attributes can be added 
class EndEl el elFinal | el -> elFinal where
  endEl :: el -> elFinal


-- ========== typed classes ========================================== 
data PT a b = PT a b -- phontom type containing state a and the result b 

 -- you should never have to create an instance for AttributeType or ElType
 -- this is done automatically from DTD
class AttributeType attrType
class ElType elType

class InitialState elType state where
  initialState :: elType -> state

class ( CreateEl elType el
      , InitialState elType initialState
      ) => CreateElT elType initialState el
      | elType -> initialState where -- el ready to append attributes 
  createElT :: elType -> PT initialState el
  createElT _  = PT (initialState (undefined :: elType))
                     (createEl (undefined :: elType))

class ( AttributeType attrType
      , AddAttribute el attrType
      , StAddAttr attrType st st2
      ) => AddAttributeT attrType st st2 el 
      | st -> st2 where 
  addAttributeT :: PT st el -> attrType -> String -> PT st2 el -- Sting = attr value FIXME: extend to all attr types - still proof of concept
  addAttributeT (PT _ t) _ v = PT (undefined :: st2)
                                  (addAttribute t (undefined :: attrType) v)
                     

-- after this no attributes can be added 
class ( EndAttrs el el2
      , StEndAttrs st st2
      ) => EndAttrsT el el2 st st2 where
  endAttrsT :: PT st el -> PT st2 el2
  endAttrsT (PT _ t) = PT (undefined :: st2) (endAttrs t)

class ( AddEl el
      , StAddEl st stc st2
      ) => AddElT el st stc st2 where
  addElT :: PT st el -> PT stc el -> PT st2 el
  addElT (PT _ t) (PT _ c) = PT (undefined :: st2) (addEl t c)
class ( AddText el text
      , StAddText st st2
      ) => AddTextT el el2 text st st2 where
  addTextT :: PT st el -> text -> PT st2 el
  addTextT (PT _ t) text = PT (undefined :: st2) (addText t text)

-- after this no attributes can be added 
class ( EndEl el el2
      , StEndAttrs st st2
      ) => EndElT el el2 st st2 where
  endElT :: (PT st el) -> (PT st2 el2)

-- ========== the ugly part, the validation by transforming states === 

data Element elType requiredAttributes addedAttrs elementsState

-- content
-- data elType
-- data Any -- any element is allowed
data Choice hlist
data Sequence hlist

-- modifier
data Elem elType
data Query content -- ^ Zero Or One
data Star content  -- ^ Zero Or More
-- after consuming one subtoken of Star StarB will contain the new state and
-- content will contain the original content, so that we can reset state after
-- all content has been consumed to start a new cycle
data StarB state content


class (AttributeType attrType
      ) => StAddAttr attrType state state2 | attrType state -> state2
class StEndAttrs el el2 | el -> el2
class StAddEl el child el2 | el child -> el2
class StAddText el el2 | el -> el2

-- state about adding other elements 

-- ========== finalization ===========================================
-- is it valid to finalize an element without adding further elements?
-- a finalized element is denoted by (Fin elType)
data Fin el
data Next el -- result type dummy so that the result doesn't match Fin 
class StValidFinalization elType st st2 | elType st -> st2
instance StValidFinalization a (Fin a) (Fin a) -- already finalized 
instance StValidFinalization a (Star b) (Fin a) -- zero or more 
instance StValidFinalization a (Query b) (Fin a) -- zero or 1 
-- task of sequence after adding last matching item..
-- instance StValidFinalization (Sequence HNil) () -- end of sequence
-- TODO think about nice error message
instance ( ShouldHaveBeenFinalizedByAddAttr a
         ) => StValidFinalization a (Sequence HNil) Error -- end of sequence
instance ( GotEndButElExpected a b
         ) => StValidFinalization a (Sequence (HCons b c)) Error -- end of sequence
instance ( GotEndButElExpectedOneOf a b
         ) => StValidFinalization a (Choice b) Error -- end of sequence

-- ========== adding element ========================================= 
{-
class StAddElement el childEl 
                   st childSt 
                   st2 | el childEl st childSt -> st2
-- single element 
instance ( StValidFinalization el childSt (Fin el)
        ) => StAddElement el childEl
                          (Elem childEl) childSt
                          (Fin el) --  state = childEl? match! 
-- sequence reduction 
instance ( StValidFinalization el childSt (Fin el) -- sequence subelement match and subelement consumed, end of sequence
        , StAddElement el childEl next childSt (Fin el)
        ) => StAddElement el childEl
                          (Sequence (HCons next HNil)) childSt
                          (Fin el) -- this sequence is consumed as well 
instance ( StValidFinalization el childSt (Fin el) -- sequence subelement match and subelement consumed, end of sequence
        , StAddElement el childEl next childSt (Fin el)
        ) => StAddElement el childEl
                          (Sequence (HCons next (HCons ta tb))) childSt
                          (Next (Sequence (HCons ta tb))) -- this sequence is not yet consumed 
instance ( StValidFinalization el childSt (Fin el) -- sequence subelement match and subelement not fully consumed
        , StAddElement el childEl next childSt (Next res)
        ) => StAddElement el childEl
                          (Sequence (HCons next tail)) childSt
                          (Next (Sequence (HCons res tail))) -- replace first sequence by new state 
-}
{-
-- choice reduction 
class ( ErrorNoMoreElementsExpectedWhenAdding el childEl -- subelement consumed 
      ) => StAddElement el childEl (Fin el) c Error
-- TODO more error messages 
class ( StValidFinalization childSt -- sequence subelement match and subelement consumed, end of sequence
      , StAddElement el childEl next childSt (Fin el)
      ) => StAddElement el childEl
                        (Sequence (HCons next HNil)) childSt
                        (Fin el) -- this sequence is consumed as well 

Query
Star
StarB
reduction
-}


-- ========== classes to enhance error messages ====================== 
data Error -- dummy 
class ShouldHaveBeenFinalizedByAddAttr a
class GotEndButElExpected a b
class GotEndButElExpectedOneOf a b
class ErrorNoMoreElementsExpectedWhenAdding a b
