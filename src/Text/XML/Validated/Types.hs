{-# LANGUAGE NoMonomorphismRestriction,  PatternSignatures, ScopedTypeVariables, EmptyDataDecls,
    FunctionalDependencies, FlexibleInstances, FlexibleContexts,
    MultiParamTypeClasses, UndecidableInstances, OverlappingInstances #-}

module Text.XML.Validated.Types (
  -- Validated -- contains the validated xml result
  -- , fromValidated  -- use this to get the result
  -- , validate -- use this to tun an element into a validated (you probably want to use (fromValidated . validate) (createElT ..) )
  fromPT
  -- your interface: (memomic: trailing T for with phantom _T_ypes or _t_yped)
  , CreateElT(..)
  , AddAttrT(..)
  , AddElT(..)
  , AddTextT(..)
  , EndElT(..)
  , XmlDocT(..)

  -- implementation for different result types: 
  , CreateEl(..)
  , AddAttribute(..)
  , EndAttrs(..)
  , EndAttrsEndElement(..)
  , AddEl(..)
  , AddText(..)
  , EndEl(..)
  , XmlDoc(..)
  , XmlIds(..)


  -- exported for utils and TH, should be moved in its own module ?
  , Seq, Or, ANY, Element, C, Query, Star, EMPTY

  , ElType
  , InitialState(..)
  , AttributeType
  , PT, NYV, Elem, TypeToNat
  , XmlIdsFromRoot(..)
  -- -- for debugging
  , Consume

  ) where 

import Data.HList
import Data.Either


-- data Validated a = Validated a
-- fromValidated (Validated a) = a
-- validate = endElT
fromPT (PT _ v) = v
{- We encode the compile time XML validation state in type as well
 First a el is created
 snd all attributes are added
 trd all subels and text elements are added
 last it is added to another el or the document

 These steps have been chosen because it's closest to String serialization
 which is the main purpose of this library
-}

-- ========== classes to generate the result type ==================== 
-- mainly targeting kind of String output (see Instances/*)

data XmlIds = XmlIds { 
  publicId :: Maybe String
  , systemId :: Maybe String
  }

class CreateEl elType el where
  createEl :: elType -> el

class (AttributeType attrType
  ) => AddAttribute el attrType where 
  addAttribute :: el -> attrType -> String -> el -- Sting = attr value FIXME: extend to all attr types - still proof of concept

-- after this no attributes can be added 
class EndAttrs el el2 | el -> el2, el2 -> el where
  endAttrs :: el -> el2
class AddEl el elc | el -> elc, elc -> el
    -- el -> elc: from a not finalized element can a finalized be deduced (maybe not necessary) 
    -- elc -> el so that you can just give the end type and the types before will be infered automatically
  where 
  addEl :: el -> elc -> el -- el, child 

{-
Empty-element tags may be used for any element which has no content, whether or
not it is declared using the keyword EMPTY. For interoperability, the
empty-element tag SHOULD be used, and SHOULD only be used, for elements which
are declared EMPTY.
-}
class EndAttrsEndElement elType el elFinal | el -> elFinal, elFinal -> el where
  endAttrsEndElement, endAttrsEndElementDeclaredEmpty :: elType -> el -> elFinal
  endAttrsEndElementDeclaredEmpty _ = endAttrsEndElement (undefined :: elType)
  endAttrsEndElement _ = endAttrsEndElementDeclaredEmpty (undefined :: elType)

class AddText el text where
  addText :: el -> text -> el -- el, text node

-- after this no attributes can be added 
class EndEl elType el elFinal | el -> elFinal, elFinal -> el where
  endEl :: elType -> el -> elFinal

class XmlDoc rootElType el el' | el -> el', el' -> el where
  xmlDoc :: rootElType -> XmlIds -> el -> el'

-- ========== typed classes ========================================== 

class XmlIdsFromRoot rootElType where
  xmlIds :: rootElType -> XmlIds -- public, system id 

class XmlDocT elst el doc |  doc -> el, el -> doc where
  xmlDocT :: (PT elst el) -> doc

-- instance ended root element 
instance ( XmlDoc rootElType el doc
        , XmlIdsFromRoot rootElType
        ) => XmlDocT (Valid rootElType) el doc where
  xmlDocT (PT _ e) = xmlDoc (undefined :: rootElType) (xmlIds (undefined :: rootElType)) e
-- instance not yet ended root elemnt 
instance ( EndElT (NYV (elType stA st hchs)) st' el el2
        , XmlDocT st' el2 doc
        ) => XmlDocT (NYV (elType stA st hchs)) el doc where
  xmlDocT = xmlDocT . endElT
               

data PT a b = PT a b -- phantom type containing state a and the result b 

 -- you should never have to create an instance for AttributeType or ElType
 -- this is done automatically from DTD in TH.hs
class AttributeType attrType
class ElType elType

class InitialState elType initialState | elType -> initialState
  where
  initialState :: elType -> initialState
  initialState = undefined

class ( InitialState elType initialState -- default instance, no need to write your own 
      , CreateEl elType el
      ) => CreateElT elType initialState el
      | elType -> initialState where -- el ready to append attributes 
  createElT :: elType -> PT initialState el
  createElT _  = PT (initialState (undefined :: elType))
                     (createEl (undefined :: elType))

instance (
      ElType elType
    , CreateEl elType el
    , InitialState elType initialState
    ) => CreateElT elType initialState el

class ( AttributeType attrType
      , AddAttribute el attrType
      , StAddAttr attrType st st2
      ) => AddAttrT attrType st st2 el 
      | st -> st2 where 
  addAttrT :: PT st el -> attrType -> String -> PT st2 el -- Sting = attr value FIXME: extend to all attr types - still proof of concept
  addAttrT (PT _ t) _ v = PT (undefined :: st2)
                          (addAttribute t (undefined :: attrType) v)


class (AttributeType attrType
      ) => StAddAttr attrType state state2 | attrType state -> state2

-- ========== type level implementation ============================== 

class HFromMaybe a b r | a b -> r
instance HFromMaybe (HJust a) b a
instance HFromMaybe HNothing b b


-- ========== adding sub elements (tags) =============================
class AddElT est est' el el2 stc elc
    | el -> el2, el2 -> el, est -> est' where
  addElT :: PT est el -> PT stc elc -> PT est' el2
-- first child ? (then endAttrs must be called)
instance (
    EndAttrs el el2
  , AddEl el2 elc
  , Consume st celType r
  , Retry r celType st'
  ) => AddElT (NYV (Element elType stA st HFalse)) (NYV (Element elType stA st' HTrue)) el el2
              (Valid celType) elc where
 addElT (PT _ t) (PT _ c) = PT (undefined :: est') $ addEl (endAttrs t) c
-- not first child
instance (
    AddEl el elc
  , Retry r celType st'
  , Consume st celType r
  ) => AddElT (NYV (Element elType stA st HTrue)) (NYV (Element elType stA st' HTrue)) el el
              (Valid celType) elc where
 addElT (PT _ t) (PT _ c)= PT (undefined :: est') $ addEl t c
-- validate child 
instance (
    AddEl el elc'
  , Retry r celType st'
  , Consume st celType r
  , EndElT (NYV (Element celType cstA cst chchs)) (Valid celType) elc elc'
  , AddElT (NYV (Element elType  stA st hchs)) (NYV (Element elType stA st' HTrue)) el el
           (Valid celType) elc'
  ) => AddElT (NYV (Element elType  stA st hchs)) (NYV (Element elType stA st' HTrue)) el el
              (NYV (Element celType cstA     cst   chchs)) elc where
 addElT el childEl = addElT el $ endElT childEl

-- ========== adding sub elements (text) ============================= 
class AddTextT el el2 text elst elst2 | el -> el2, el2 -> el, elst -> elst2 where
  addTextT :: PT elst el -> text -> PT elst2 el2
-- first child 
instance (
    EndAttrs el el2
  , StEndAttrs stA
  , AddText el2 text
  ) => AddTextT el el2 text (NYV (Element elType stA st  HFalse))
                            (NYV (Element elType stA st' HTrue))
  where
  addTextT (PT _ t) text = PT undefined $ addText (endAttrs t) text
-- not first child
instance ( AddText el text
  ) => AddTextT el el text (NYV (Element elType stA st  HTrue))
                           (NYV (Element elType stA st HTrue))
  where
  addTextT (PT _ t) text = PT undefined $ addText t text

-- ========== final element check ==================================== 
class EndElT st st' el el2| st -> st', el -> el2, el2 -> el where
    endElT :: (PT st el) 
           -> (PT st' el2)
-- end element with childs 
instance ( EndEl elType el el2
         , ElEndable st
         ) => EndElT (NYV (Element elType stA st HTrue))
                     (Valid elType) el el2
  where endElT (PT _ el) = PT undefined (endEl (undefined :: elType)  el)
-- end elements without childs declared EMPTY 
instance ( EndAttrsEndElement elType el el2
         , StEndAttrs stA
         ) => EndElT (NYV (Element elType stA EMPTY HFalse))
                     (Valid elType) el el2
  where endElT (PT _ el) = PT undefined (endAttrsEndElementDeclaredEmpty (undefined :: elType) el)
-- end elements without childs not declared EMPTY 
-- the following instances only differ in st, when using overlapping instances one would suffice
instance ( EndAttrsEndElement elType el el2
         , StEndAttrs stA
         -- , ElEndable st
         ) => EndElT (NYV (Element elType stA C HFalse))
                     (Valid elType) el el2
  where endElT (PT _ el) = PT undefined (endAttrsEndElement (undefined :: elType) el)
instance ( EndAttrsEndElement elType el el2
         , StEndAttrs stA
         -- , ElEndable st
         ) => EndElT (NYV (Element elType stA (Star a) HFalse))
                     (Valid elType) el el2
  where endElT (PT _ el) = PT undefined (endAttrsEndElement (undefined :: elType) el)
instance ( EndAttrsEndElement elType el el2
         , StEndAttrs stA
         -- , ElEndable st
         ) => EndElT (NYV (Element elType stA (Query a) HFalse))
                     (Valid elType) el el2
  where endElT (PT _ el) = PT undefined (endAttrsEndElement (undefined :: elType) el)
  -- fail nicer error messages 
instance ( EndAttrsEndElement elType el el2
         , StEndAttrs stA
         -- , ElEndable st
         , MoreElementsExpected (Elem a)
         ) => EndElT (NYV (Element elType stA (Elem a) HFalse))
                     (Valid elType) el el2
  where endElT _ = undefined -- shut up warning
instance ( EndAttrsEndElement elType el el2
         , StEndAttrs stA
         -- , ElEndable st
         , MoreElementsExpected (Seq a b)
         ) => EndElT (NYV (Element elType stA (Seq a b) HFalse))
                     (Valid elType) el el2
  where endElT _ = undefined -- shut up warning
instance ( EndAttrsEndElement elType el el2
         , StEndAttrs stA
         -- , ElEndable st
         , MoreElementsExpected (Or a b)
         ) => EndElT (NYV (Element elType stA (Or a b) HFalse))
                     (Valid elType) el el2
  where endElT _ = undefined -- shut up warning

-- ========== the uglier part, the validation by transforming states

-- ========== attributes stuff =======================================
-- add attribute to added list, fail if it has already been added
-- remove attr from recquired list, add it to attrs list 
instance (
    AttributeType attrType
  , HMemberM attrType req r -- remove attr 
  , HFromMaybe r req req'
  , HMember added attrType HFalse -- don't allow adding elements twice 
  ) => StAddAttr attrType
                 (NYV (Element elType (req, added) st hcs))
                 (NYV (Element elType (req', (HCons attrType added)) st hcs))

-- ========== elements and subelements / consume classes =============

-- element content accept stuff
data Element elType stAttributes elementsState hasChilds 
-- elType: the element type_T 
-- hasChilds: HTrue or HFalse, is necessary to to know wether empty tags <br/> can be used

-- ========== allowed to end element ? =============================== 
class ElEndable a
instance ElEndable C
instance ElEndable EMPTY
instance ElEndable (Star a)
instance ElEndable (Query a)
  -- fail nicer error messages 
instance ( MoreElementsExpected (Elem a)) => ElEndable (Elem a)
instance ( MoreElementsExpected (Seq a b)) => ElEndable (Seq a b)
instance ( MoreElementsExpected (Or a b))  => ElEndable (Or a b)

class StEndAttrs elst
instance  StEndAttrs (HNil, added)
-- instance  StEndAttrs () -- already ended 
  -- fail nicer error messages
instance  ( RequiredAttributesMissing (HCons a b)
          ) => StEndAttrs ((HCons a b), added)

-- ========== adding elements etc .. ================================= 

class StAddEl st st2 celType | st celType -> st2
instance  (
    Consume st celType r
  , Retry r celType st'
  ) => StAddEl st celType st'


-- retry errors and retries consuming element on result (R x) 
class Retry st el st' | st el -> st'
instance Retry C el C -- alread done, nothing can be added
instance Retry (CS st) el st -- alread done, continue with state st
instance ( -- retry 
  Consume st el r
  , Retry r el st'
  ) => Retry (R st) el  st'
  -- fail nicer error messages 
instance ( NoMoreElementsExpectedOrWrongElement el
         ) => Retry RSKIP el ()
instance ( Fail x ) => Retry (F x) el ((),())


-- content
-- data elType
-- data Any -- any element is allowed
data Or a tail
-- the last two elementes are Sequence elem elem, thus a sequence with one element is not possible 
-- a list with three elements looks like this: Seq a ( Seq b c)
data Seq a tail

data EMPTY         -- see comment on EndAttrsEndElement 
data ANY           -- any element 
data Elem elType   -- match an element 
-- modifier
data Query content -- ^ Zero Or One
data Star content  -- ^ Zero Or More
-- after consuming one subtoken of Star StarB will contain the new state and
-- content will contain the original content, so that we can reset state after
-- all content has been consumed to start a new cycle
data StarB state content

data Validated -- state will be set to Validated after endElT 
data NYV state -- not yet valid state 
data Valid elType -- "state" of validated element
data C      -- consumed, no element left
data CS a   -- consume success, new state is a
data R a    -- retry with state a (happens after removing ()* if content didn't match)
data RSKIP  -- retry, but skip this content (no match on ()*) 
data F a    -- consume failure, a is reason

class Consume st el r | st el -> r -- result is on of C,CS,R,F 

-- element 
instance Consume ANY el C
instance ( TypeToNat el elNr
         , TypeToNat el' elNr'
         , HEq elNr elNr' r'
         , HCond r' C (F (ExpectedButGot el el')) r
         ) => Consume (Elem el) el' r

-- list consumption
class LConsume isConsumed b r | isConsumed b -> r 
instance LConsume C            b (CS b)        -- head consumed, return tail
instance LConsume (CS a)       b (CS (Seq a b))-- head consumed element, but there are more to be consumed
instance LConsume (F x)        b (F x)         -- head failed consuming element
instance LConsume (RSKIP)      b (R b)         -- retry with head skipped
instance LConsume (R a)        b (R (Seq a b)) -- retry with new head state eg add b to head (a*,b), new head will become just b

instance ( Consume a el r
         , LConsume r b res
         ) => Consume (Seq a b) el res

-- choice consumption
--  choice helper funtion
{- all cases
 let l = [ "C", "(CS a)", "(R a)", "(RSKIP)", "(F a)"]
 mapM_ putStrLn [ "instance TakeConsumed " ++ a ++ " " ++ b | a <- l, b <- l]
 -}
class TakeConsumed a b r | a b -> r -- helper function 
-- dtd must be deterministic, one C, CS is enough to skip the other
instance TakeConsumed C a C
instance TakeConsumed (CS a) C C
instance TakeConsumed (R a) C C
instance TakeConsumed (RSKIP) C C
instance TakeConsumed (F a) C C
instance TakeConsumed (CS a) (F b) (CS a)
instance TakeConsumed (F b) (CS a) (CS a)
-- instance TakeConsumed (R b) (CS a) (CS a) -- cannot occcur (dtd determinism) 
-- instance TakeConsumed RSKIP (CS a) (CS a) -- cannot occour 


instance TakeConsumed (RSKIP) (RSKIP) RSKIP

instance TakeConsumed (F a) (RSKIP) RSKIP
instance TakeConsumed (RSKIP) (F a) RSKIP

instance TakeConsumed (R a) (RSKIP) (Query a)
instance TakeConsumed (RSKIP) (R a) (Query a)

instance TakeConsumed (R a) (R b) (R (Or a b))
instance TakeConsumed (R a) (F b) (R a)
instance TakeConsumed (F a) (R b) (R b)
instance TakeConsumed (F a) (F b) (F (BothFailed a b))


instance ( Consume a el r
         , Consume b el r2
         , TakeConsumed r r2 res
         ) => Consume (Or a b) el res
-- modifier 
-- Query 
class QConsume r res | r -> res -- helper function 
instance QConsume C C
instance QConsume (CS a) (CS a)
instance QConsume (R a)  (Query (R a))
instance QConsume (RSKIP) RSKIP
instance QConsume (F a)   RSKIP
instance ( Consume a el r
         , QConsume r res
         ) => Consume (Query a) el res
-- Star
class SConsume a r res | a r -> res -- helper function 
instance SConsume a C (CS (Star a)) -- next cycle 
instance SConsume a (CS b) (CS (StarB b a)) -- backup content 
instance SConsume a (R b)  (R (StarB b a))
instance SConsume a (RSKIP) RSKIP
instance SConsume a (F a)   RSKIP
instance ( Consume a el r
         , SConsume a r res
         ) => Consume (Star a) el res

-- StarB
class SBConsume a r res | r -> res -- helper function 
instance SBConsume b C      (CS (Star b)) -- next cycle 
instance SBConsume b (CS a) (CS (StarB a b)) --  
instance SBConsume b (R  c) (R (StarB c b))
instance SBConsume b (RSKIP) RSKIP
instance SBConsume b (F a)   (F a)

instance ( Consume a el r
         , SBConsume b r res
         ) => Consume (StarB a b) el res

-- ========== errors ================================================= 
data BothFailed a b
data ExpectedButGot a b

-- never implement instances for these.. either the lib is buggy or your data
-- does'nt validate against dtd 
class MoreElementsExpected a
class RequireAttrubtes a
class NoMoreElementsExpectedOrWrongElement a
class RequiredAttributesMissing req
-- class Fail (from HList) 

--  ========= type level equality helper =============================
class TypeToNat typE nr | typE -> nr
