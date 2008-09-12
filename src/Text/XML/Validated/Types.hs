{-# OPTIONS_GHC -fcontext-stack=100 #-}
{-# LANGUAGE NoMonomorphismRestriction,  ScopedTypeVariables, EmptyDataDecls,
    FunctionalDependencies, FlexibleInstances, FlexibleContexts,
    MultiParamTypeClasses, UndecidableInstances #-}

{- see cabal file
#ifndef TypeToNatTypeEq
  {-# Language OverlappingInstances  #-}
#endif
-}

module Text.XML.Validated.Types (
  -- your interface: (memomic: trailing T for with phantom _T_ypes or _t_yped)
    AddAttrT(..)  -- addAttrT: use the function generation by template haskell instead called "attrname_A"
  , (<<) -- may change
  , AddElT(..)    -- addElT: add sub element. alias (<<)
  , (<<<) --  may change
  , AddTextT(..)  -- addTextT: add PCDATA to element. alias (<<<)
  , EndElT(..)
  -- , XmlDocT(..) -- use xml instead
  , xml    -- validate, return result type of root element with doctype
  , fromPT -- validate, return result type of any non root element
  -- , text -- flip addTextT
  -- monadic like functions  (see test.hs for a convinient usage with help of cpp)
  , VXMLMonad(..), VXML(..), VXMLT(..), VXMLReturn(..), vxmllift, VXMLText(..)

  -- implementation for different result types:
  -- see example in Instances
  , CreateEl(..)
  , AddAttribute(..)
  , EndAttrs(..)
  , EndAttrsEndElement(..)
  , AddEl(..)
  , AddText(..)
  , EndEl(..)
  , XmlDoc(..)
  , XmlIds(..)
  , DetermineEl
  , DetermineElAddEl


  -- exported for utils and TH, you should not have to use them
  , Element, AS, AttrOk
#ifdef DoValidate
  , ANY, C, PCDATA, A, ElEndable
#else
  , NoValidation
#endif
  , EMPTY

  , PT(..), NYV, Elem, Valid
#ifdef TypeToNatTypeEq
  , TypeToNat
#endif

  , XmlIdsFromRoot(..)
  -- for debugging
  , debugEl
  , fromPTInternal
#ifdef DoValidate
  , Consume
#endif
  ) where

import Data.HList
import Data.Either

{- design notes
   ============================================================================

   the element validation state which keeps information about which attributes
   and elements may still be added is either

   NYV ( -- NYV = not yet valid
        Element
          elType          -- the eltype (Html_T or Body_T ..)
          stateAttributes  -- see Consume for a list of valid content
          stateChilds     -- (AS required added) all three beeing a HList of (A attrtype)
          hasChildren     -- HTrue or HFalse
       )

   or

   Valid ( elType )

  The life cycle of the state is typically:
  -----------------------------------------

  NYV ( Element elType stA st HFalse )

  -- addAttrT: add some attributes adding the attributes to the added list and removing
  -- them from the required list
  NYV ( Element elType stA' st HFalse )

  -- addElT: add one element, Consume will determine the new state,
  -- fail if the child (elment or PCDATA) may not be added attribute validation
  -- takes here or if no child is added in endElT
  NYV ( Element elType stA' st' HTrue )

  --  endElT: validate children state
  Valid elType

  For elements without childs the shortcut EndAttrsEndElement will be used to
  allow easy generation of empty <img .../> text instead of <img ...></img>


  Generation of the result type:
  -----------------------------------------
  Each time when one of createElT, addAttrT, endElT is called a corresponding
  function is called to create the result type. The reference implementation
  will result in a ready to use XML-String. See Instances / *.hs

  That's why an element has the type PT (state resultType)

  It should be sufficiently easy to implement returning kind of tree
  representation as well.

-}



-- see also xml
fromPT = fromPTInternal . endElT

-- fromPTInternal: take result type before final validation (done in endElT), internal
-- use only
fromPTInternal :: (PT st el) -> el
fromPTInternal (PT _ v) = v

-- xml is alias for xmlDocT. xmlDocT is the same as fromPT but it also adds the
-- doc type declaration
xml = xmlDocT
(<<) = addElT
(<<<) = addTextT

-- ========== monadic interface ======================================
{- why ? because the do sugar notation is the only one I know about which
   can concatenate elements at the same level and add sublevel elemnts easily

   Have a look at test.hs for an example on how to use it

   What a pity, the standard monat cannot be used as
   (>>) :: m a -> m b -> m b
   Indeed we need something like
   (>>) :: m a -> m' b -> m'' b

   Hopefully we can use rebindable syntax to make this work
-}

class VXMLMonad m  st el  st2 el2  st3 el3  st4 el4
    | el2 st st2 -> el
    , st st3 el3 -> el
    , st st3 el3 -> el
    , st st4 el4 -> el
    , st st2 el2 -> el
    , st st4 el4 -> el
    where
  vxmlgtgt ::  m st el  st2 el2  st3 el3  a
            -> m st el  st3 el3  st4 el4  b
            -> m st el  st2 el2  st4 el4  b
  vxmlgtgt a b = vxmlbind a $ const b
  vxmlbind :: m st el  st2 el2  st3 el3 a
              -> (a -> m st el  st3 el3  st4 el4  b)
              -> m st el  st2 el2  st4 el4  b
class VXMLReturn m where
  vxmlreturn :: a -> m  st el  st2 el2  st2 el2  a

data VXML st el     -- input element
          st2 el2   -- type of result of passed function
          st3 el3 a --  type of result of resulting function
          = VXML {
          runVVXML ::     (PT st el -> PT st2 el2)
                    -> (a, PT st el -> PT st3 el3)
          }

instance VXMLMonad VXML st el  st2 el2  st3 el3  st4 el4 where
  vxmlbind (VXML f1) f = VXML $ \p ->
                            let (a,p2) =  f1 p
                            in runVVXML (f a) p2
instance VXMLReturn VXML where
  vxmlreturn a = VXML $ \p -> (a,p)

-- with inner monad
data VXMLT m st el  st2 el2  st3 el3 a  = VXMLT {
          runVVXMLT ::       (PT st el -> PT st2 el2)
                    -> m (a, PT st el -> PT st3 el3)
          }
instance (Monad m
        ) => VXMLMonad (VXMLT m)
                  st el  st2 el2  st3 el3  st4 el4 where
  vxmlbind (VXMLT f1) f = VXMLT $ \p -> do
        (a, p2) <- f1 p
        runVVXMLT (f a) p2

instance (Monad m) => VXMLReturn (VXMLT m) where
  vxmlreturn a = VXMLT $ \p -> return (a, p)

-- lift
vxmllift :: (Monad m) => m a -> VXMLT m st el  st2 el2  st2 el2 a
vxmllift f = VXMLT $ \p -> do
  a <- f
  return (a, p)

class VXMLText text m st el st2 el2 st3 el3
      | st2 -> st3
      , st2 st3 el3 -> el2  where
      text :: text -> m st el st2 el2 st3 el2 ()
instance ( AddTextT el2 el2 text st2 st3
        ) => VXMLText text VXML st el st2 el2 st3 el2 where 
  text t = VXML $ \p -> ((),(`addTextT` t) . p)

instance ( Monad m
          , AddTextT el2 el2 text st2 st3
          ) => VXMLText text (VXMLT m) st el st2 el2 st3 el3 where 
  text t = VXMLT $ \p -> return ((),(`addTextT` t) . p)

-- ========== classes to generate the result type ====================
-- mainly targeting kind of String output (see Instances/ * )

-- used for AddAttrT
class DetermineEl st el st2 el2 | st st2 el2 -> el
class DetermineElAddEl st el st2 el2 stc elc

      | st st2 stc el2 -> el
      , st st2 stc el2 -> elc

data XmlIds = XmlIds {
  publicId :: Maybe String
  , systemId :: Maybe String
  }

class CreateEl elType el where
  createEl :: elType -> el

class AddAttribute el el2 attrType attrValue where
  addAttribute :: el -> attrType -> attrValue -> el2

-- after this no attributes can be allowed
class EndAttrs el el2 where
  endAttrs :: el -> el2
class AddEl el elc | el -> elc, elc -> el
    -- el -> elc: from a not finalized element can a finalized be deduced (maybe not necessary)
    -- elc -> el so that you can just give the end type and the types before will be infered automatically
  where
  addEl :: el -> elc -> el -- el, child

{- quote from XML spec:
  Empty-element tags may be used for any element which has no content, whether or
  not it is declared using the keyword EMPTY. For interoperability, the
  empty-element tag SHOULD be used, and SHOULD only be used, for elements which
  are declared EMPTY
-}
class EndAttrsEndElement elType el elFinal
  where
  endAttrsEndElement, endAttrsEndElementDeclaredEmpty :: elType -> el -> elFinal
  endAttrsEndElementDeclaredEmpty _ = endAttrsEndElement (undefined :: elType)
  endAttrsEndElement _ = endAttrsEndElementDeclaredEmpty (undefined :: elType)

class AddText el text where
  addText :: el -> text -> el -- el, text node

-- after this no attributes can be allowed
class EndEl elType el elFinal where
  endEl :: elType -> el -> elFinal

class XmlDoc rootElType el el' | el -> el', el' -> el where
  xmlDoc :: rootElType -> XmlIds -> el -> el'

-- ========== type level implementation ==============================


class XmlIdsFromRoot rootElType where
  xmlIds :: rootElType -> XmlIds -- public, system id

-- TODO add class constraint to determine el from st and doc
class XmlDocT elst el doc |  doc elst -> el where
  xmlDocT :: (PT elst el) -> doc

-- instance ended root element
instance ( XmlDoc rootElType el doc
        , XmlIdsFromRoot rootElType
        ) => XmlDocT (Valid rootElType) el doc where
  xmlDocT (PT _ e) = xmlDoc (undefined :: rootElType) (xmlIds (undefined :: rootElType)) e
-- instance not yet ended root elemnt
instance ( EndElT (NYV (Element elType stA st hchs)) el st' el
         ,XmlDocT st' el doc
        ) => XmlDocT (NYV (Element elType stA st hchs)) el doc where
  xmlDocT = xmlDocT . endElT


data PT a b = PT a b -- phantom type containing state a and the result b

class AttrOk elType attr
class AddAttrT attrType attrValue st el st2 el2
      | st attrType -> st2
      , st st2 el2 -> el
  where
  addAttrT :: PT st el -> attrType -> attrValue -> PT st2 el2

instance (
        AddAttribute el el2 attrType attrValue
      , AttrOk elType attrType
#ifdef DoValidate
      , -- remove attribute from required list
        HMemberM (A attrType) req mreq
      , HFromMaybe mreq req req'
      , -- no attribute may be added twice
        HMember (A attrType) added rd
      , CheckDuplicateAttribute elType attrType rd
#else
      , IdClass req req'
#endif
      , DetermineEl (AS req  added) el
                    (AS HNil (HCons (A attrType) added)) el2
      ) => AddAttrT attrType attrValue
                    (NYV (Element elType (AS req  added) st HFalse)) el
                    (NYV (Element elType (AS HNil (HCons (A attrType) added)) st HFalse)) el2


  where
  addAttrT (PT _ t) _ v = PT (undefined :: st2)
                          (addAttribute t (undefined :: attrType) v)
     -- fail nicer error messages
instance ( YouCantAddAttributesAfterAddingContentTo elType
      ) => AddAttrT attrType attrValue (NYV (Element elType stA st HTrue)) el st2 el2
  where addAttrT = undefined -- shut up warning


#ifdef DoValidate

class HFromMaybe mb b r | mb b -> r
instance  HFromMaybe HNothing b b
instance  HFromMaybe (HJust a) b a
class CheckDuplicateAttribute elType attrType hbool
instance CheckDuplicateAttribute elType attrType HFalse
instance ( DuplicateAttribute elType attrType
         ) => CheckDuplicateAttribute elType attrType HTrue

#endif

-- ========== adding sub elements (tags) =============================
class (
  DetermineElAddEl est el estc elc est2 el2
  ) => AddElT est el estc elc est2 el2
    | estc est -> est2
    , est est2 estc el2 -> el
    , est est2 estc el2 -> elc
    where
  addElT :: PT est el -> PT estc elc -> PT est2 el2
-- first child ? (then endAttrs must be called)
instance (
    EndAttrs el el2
  , AddEl el2 elc
#ifdef DoValidate
  , Consume st (Elem celType) st'
#else
  , IdClass st st'
#endif
  , DetermineElAddEl (NYV (Element elType stA st HFalse)) el
                     (Valid celType) elc
                     (NYV (Element elType stA st' HTrue)) el2
  ) => AddElT (NYV (Element elType stA st HFalse)) el
              (Valid celType) elc
              (NYV (Element elType stA st' HTrue)) el2
              where
 addElT (PT _ t) (PT _ c) = PT (undefined :: est') $ addEl (endAttrs t) c
-- not first child
instance (
    AddEl el elc
#ifdef DoValidate
  , Consume st (Elem celType) st'
#else
  , IdClass st st'
#endif
  , DetermineElAddEl (NYV (Element elType stA st HTrue)) el
                     (Valid celType) elc
                     (NYV (Element elType stA st' HTrue)) el
  ) => AddElT (NYV (Element elType stA st HTrue)) el
              (Valid celType) elc
              (NYV (Element elType stA st' HTrue)) el
              where
 addElT (PT _ t) (PT _ c)= PT (undefined :: est') $ addEl t c
-- validate child
instance (
    AddEl el elc'
#ifdef DoValidate
  , Consume st (Elem celType) st'
#else
  , IdClass st st'
#endif
  , EndElT (NYV (Element celType cstA cst chchs)) elc
           (Valid celType) elc
  , AddElT (NYV (Element elType  stA   st  hchs )) el
           (Valid celType) elc
           (NYV (Element elType  stA   st' HTrue)) el2
  , DetermineElAddEl (NYV (Element elType stA st hchs)) el
                     (NYV (Element celType cstA cst chchs)) elc
                     (NYV (Element elType stA st' HTrue)) el2
  ) => AddElT (NYV (Element elType  stA st hchs)) el
              (NYV (Element celType cstA     cst   chchs)) elc
              (NYV (Element elType stA st' HTrue)) el2

  where
 addElT el childEl = addElT el $ endElT childEl

-- ========== adding sub elements (text) =============================
class AddTextT el el2 text elst elst2 | el -> el2, el2 -> el, elst -> elst2 where
  addTextT :: PT elst el -> text -> PT elst2 el2
-- first child
instance (
    EndAttrs el el2
#ifdef DoValidate
  , StEndAttrs elType stA
#endif
  , AddText el2 text
#ifdef DoValidate
  , Consume st PCDATA st'
#else
  , IdClass st st'
#endif
  ) => AddTextT el el2 text (NYV (Element elType stA st  HFalse))
                            (NYV (Element elType stA st' HTrue))
  where
  addTextT (PT _ t) text = PT undefined $ addText (endAttrs t) text
-- not first child
instance (
    AddText el text
#ifdef DoValidate
  , Consume st PCDATA st'
#else
  , IdClass st st'
#endif
  ) => AddTextT el el text (NYV (Element elType stA st  HTrue))
                           (NYV (Element elType stA st' HTrue))
  where
  addTextT (PT _ t) text = PT undefined $ addText t text

-- ========== final element check ====================================
class EndElT st el st2 el2
      | st -> st2
      , st st2 el2 -> el
    where
    endElT :: (PT st el)
           -> (PT st2 el2)
-- end element with childs
instance ( EndEl elType el el2
#ifdef DoValidate
         , ElEndable elType st
#endif
         , DetermineEl (NYV (Element elType stA st HTrue)) el
                             (Valid elType) el2
         ) => EndElT (NYV (Element elType stA st HTrue)) el
                     (Valid elType) el2
  where endElT (PT _ el) = PT undefined (endEl (undefined :: elType)  el)
-- end elements without childs declared EMPTY
instance ( EndAttrsEndElement elType el el2
#ifdef DoValidate
         , StEndAttrs elType stA
#endif
         , DetermineEl (NYV (Element elType stA EMPTY HFalse)) el
                       (Valid elType) el2
         ) => EndElT (NYV (Element elType stA EMPTY HFalse)) el
                     (Valid elType) el2
  where endElT (PT _ el) = PT undefined (endAttrsEndElementDeclaredEmpty (undefined :: elType) el)
-- end elements without childs not declared EMPTY
#ifdef DoValidate
instance ( EndAttrsEndElement elType el el2
         , StEndAttrs elType stA
         , ElEndable elType st
         , DetermineEl (NYV (Element elType stA st HFalse)) el
                       (Valid elType) el2
         ) => EndElT (NYV (Element elType stA st HFalse)) el
                     (Valid elType) el2
  where endElT (PT _ el) = PT undefined (endAttrsEndElement (undefined :: elType) el)
#else
instance ( EndAttrsEndElement elType el el2
         , DetermineEl (NYV (Element elType stA NoValidation HFalse)) el
                       (Valid elType) el2
         ) => EndElT (NYV (Element elType stA NoValidation HFalse)) el
                     (Valid elType) el2
  where endElT (PT _ el) = PT undefined (endAttrsEndElement (undefined :: elType) el)
#endif

-- ========== the uglier part, the validation by transforming states

-- ========== elements and subelements / consume classes =============

-- element content accept stuff
data Element elType stAttributes elementsState hasChilds
data AS req added -- attribute state
-- elType: the element type_T
-- hasChilds: HTrue or HFalse, is necessary to to know wether empty tags <br/> can be used

-- ========== allowed to end element ? ===============================
#ifdef DoValidate
class ElEndable elType a
instance ElEndable elType C
instance ElEndable elType EMPTY
instance ElEndable elType ANY
instance ElEndable elType PCDATA
  -- fail nicer error messages
instance ( MoreElementsExpected elType (Elem a)) =>  ElEndable elType (Elem a)

class StEndAttrs elType elst
instance  StEndAttrs elType (AS HNil added)
  -- fail nicer error messages
instance  ( RequiredAttributesMissing elType (HCons a b)
          ) => StEndAttrs elType (AS (HCons a b) added)
#endif

data EMPTY         -- see comment on EndAttrsEndElement
data ANY           -- any element
data Elem elType   -- match an element
data A a
data PCDATA        -- add text
data NYV state -- not yet valid state
data Valid elType -- "state" of validated element
#ifdef DoValidate
data C      -- consumed, no element left
data F a    --

class Consume st el r | st el -> r -- result is on of C,CS,R,F

-- PCDATA
instance Consume PCDATA PCDATA C
instance Consume ANY PCDATA C
instance Consume ANY (Elem e) C
-- fail nicer error messages
instance (Fail (GotPCDATAButExpected (Elem e))
        ) => Consume (Elem e) PCDATA ()

instance (Fail (ExpectedPCDATABUtGot (Elem e))
        ) => Consume PCDATA (Elem e) ()

#endif

-- ========== errors =================================================
data GotPCDATAButExpected a
data ExpectedPCDATABUtGot a

-- never implement instances for these.. either the lib is buggy or your data
-- does'nt validate against dtd
class MoreElementsExpected elType a
class RequiredAttributesMissing elType req
class YouCantAddAttributesAfterAddingContentTo elType
class DuplicateAttribute elType attrType
-- class Fail (from HList)
debugEl :: (Fail x) => (PT x String) -> b
debugEl = undefined

--  ========= type level equality helper =============================
instance ( TypeEq a b r) => HEq (A a) (A b) r

class TypeCast   a b   | a -> b, b->a   where typeCast   :: a -> b
class TypeCast'  t a b | t a -> b, t b -> a where typeCast'  :: t->a->b
class TypeCast'' t a b | t a -> b, t b -> a where typeCast'' :: t->a->b
instance TypeCast'  () a b => TypeCast a b where typeCast x = typeCast' () x
instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
instance TypeCast'' () a a where typeCast'' _ x  = x

instance (HBool b, TypeCast HFalse b
          ) => TypeEq x y b
instance TypeEq x x HTrue
#ifndef DoValidate
class IdClass a b | a -> b
instance IdClass  a a
data NoValidation
#endif
