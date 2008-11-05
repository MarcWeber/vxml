{-# OPTIONS_GHC -fcontext-stack=100 #-}
{-# LANGUAGE TypeSynonymInstances,  NoMonomorphismRestriction,  ScopedTypeVariables, EmptyDataDecls,
    FunctionalDependencies, FlexibleInstances, FlexibleContexts,
    MultiParamTypeClasses, UndecidableInstances, Rank2Types #-}

module Text.XML.Validated.Types (
  -- your interface: (memomic: trailing T for with phantom _T_ypes or _t_yped)
    AddAttrT(..)  -- addAttrT: use the function generation by template haskell instead called "attrname_A"
  , (<<) -- may change (TODO tidy up)
  , AddElT(..)    -- addElT: add sub element. alias (<<)
  , (<<<) --  may change (TODO tidy up)
  , AddTextT(..)  -- addTextT: add PCDATA to element. alias (<<<)
  , EndElT(..)
  , EndAttrsT(..)
  , XmlDocT(..) -- use alias xml instead
  , xml    -- validate, return result type of root element with doctype
  , fromPT -- validate, return result type of any non root element
  -- , text -- flip addTextT
  -- monadic like functions  (see test.hs for a convinient usage with help of cpp)
  , VXMLMonad(..), VXML(..), VXMLT(..), VXMLReturn(..), vxmllift, VXMLText(..), ForceElements(..)
  , VXMLDebug(..) -- show types of VXML monad
  , vxmlSeq_, vxmlSeqPlus_, vxmlMapSeq_, vxmlMapSeqPlus_

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
  , InitialState, CreateElT(..)
  , Element, AS, AttrOk
  , ANY, PCDATA
  , A
#ifdef WEAK_VALIDATION
  , WeakValidation
  , ChildOk
#else
  , C, ElEndable
  , Consume
#endif
  , EMPTY

  , PT(..), NYV, Elem, Valid
  , XmlIdsFromRoot(..)
  -- for debugging
  , debugEl
  , fromPTInternal
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
fromPTInternal (PT v) = v

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

class VXMLMonad m  st el_  st2 el2_  st3 el3_  st4 el4_
    where
  vxmlgtgt ::  m st el_  st2 el2_  st3 el3_  a
            -> m st el_  st3 el3_  st4 el4_  b
            -> m st el_  st2 el2_  st4 el4_  b
  vxmlgtgt a b = vxmlbind a $ const b
  vxmlbind :: m st el_  st2 el2_  st3 el3_ a
              -> (a -> m st el_  st3 el3_  st4 el4_  b)
              -> m st el_  st2 el2_  st4 el4_  b


class (StEndAttrs elType stA
  ) => ForceElements m elType stA st hchs where
  forceElements :: m elst el_ 
                     (NYV (Element elType stA st hchs) ) el_
                     (NYV (Element elType AttrsOk st HTrue) ) el_
                     ()
instance ( StEndAttrs elType stA
   ) => ForceElements VXML elType stA st hchs where
  forceElements = VXML $ \f -> ((), \pt-> let (PT el_) = f pt in  PT el_)
instance ( StEndAttrs elType stA
         , Monad m
 ) => ForceElements (VXMLT m) elType stA st hchs where
  forceElements = VXMLT $ \f -> return ((), \pt-> let (PT el_) = f pt in  PT el_)

class VXMLDebug m  st el_  st2 el2_  st3 el3_  a  where
  vxmldebug :: a -> m  st el_  st2 el2_  st3 el3_  a
  vxmldebug = undefined
instance (Fail (m st el  st2 el2  st3 el3  a))
   => VXMLDebug m  st el  st2 el2  st3 el3  a

class VXMLReturn m where
  vxmlreturn :: a -> m  st el_  st2 el2_  st2 el2_  a

data VXML st el_     -- input element
          st2 el2_   -- type of result of passed function
          st3 el3_ a --  type of result of resulting function
          = VXML {
          runVVXML ::     (PT st el_ -> PT st2 el2_)
                    -> (a, PT st el_ -> PT st3 el3_)
          }

instance VXMLMonad VXML st el  st2 el2  st3 el3  st4 el4 where
  vxmlbind (VXML f1) f = VXML $ \p ->
                            let (a,p2) =  f1 p
                            in runVVXML (f a) p2
instance VXMLReturn VXML where
  vxmlreturn a = VXML $ \p -> (a,p)

-- with inner monad
data VXMLT m st el_  st2 el2_  st3 el3_ a  = VXMLT {
          runVVXMLT ::       (PT st el_ -> PT st2 el2_)
                    -> m (a, PT st el_ -> PT st3 el3_)
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

--  purpose of *Plus variants see testXHTMTL, the trouble is eg that XHTML defines
class VXMLText text m st el_ st2 el2_ st3 el3_
      | st2 -> st3
      , st2 st3 el3_ -> el2_
      where
      text :: text -> m st el_ st2 el2_ st3 el2_ ()
instance ( AddTextT el2 el2 text st2 st3
        ) => VXMLText text VXML st el st2 el2 st3 el2 where 
  text t = VXML $ \p -> ((),(`addTextT` t) . p)

instance ( Monad m
          , AddTextT el2 el2 text st2 st3
          ) => VXMLText text (VXMLT m) st el st2 el2 st3 el3 where 
  text t = VXMLT $ \p -> return ((),(`addTextT` t) . p)


--  ul as (li)+. Thus you have St1 :li  -> St2,  St2 (endable): li -> St2 to encode the plus.
--  so the first li function must have a different type from the second.
vxmlSeq_ = foldr1 (\a n -> a `vxmlgtgt` n)
vxmlSeqPlus_ (f,fl) = f `vxmlgtgt` vxmlSeq_ fl

vxmlMapSeq_ f = vxmlSeq_ . map f
vxmlMapSeqPlus_ :: ( VXMLMonad m  st el  st2 el2  st3 el3  st3 el3
                   , VXMLMonad m  st el  st3 el3  st3 el3  st3 el3
                   ) => (forall st' elA st'' elB . t -> m  st el  st' elA  st'' elB ())
                  -> [t]
                  -> m st el st2 el2 st3 el3 ()
vxmlMapSeqPlus_ f (x:xs) = f x `vxmlgtgt` (foldr1 vxmlgtgt $ map f xs)
vxmlMapSeqPlus_ _ [] = error "vxmlSeqPlus has been called with empty list"


-- ========== classes to generate the result type ====================
-- mainly targeting kind of String output (see Instances/ * )

-- used for AddAttrT
class DetermineEl est el_ est2 el2_ | est est2 el2_ -> el_
class DetermineElAddEl est el_ estc elc_ est2 el2_
      | est est2 estc el2_ -> el_
      , est est2 estc el2_ -> elc_

data XmlIds = XmlIds {
  publicId :: Maybe String
  , systemId :: Maybe String
  }

class CreateEl elType el_ where
  createEl :: elType -> el_

class AddAttribute el_ el2_ attrType attrValue where
  addAttribute :: el_ -> attrType -> attrValue -> el2_

-- after this no attributes can be allowed
class EndAttrs el_ el2_ where
  endAttrs :: el_ -> el2_
class AddEl el_ el2_ elc_ where
  addEl :: el_ -> elc_ -> el2_ -- el, child

{- quote from XML spec:
  Empty-element tags may be used for any element which has no content, whether or
  not it is declared using the keyword EMPTY. For interoperability, the
  empty-element tag SHOULD be used, and SHOULD only be used, for elements which
  are declared EMPTY
-}
class EndAttrsEndElement elType el_ elFinal
  where
  endAttrsEndElement, endAttrsEndElementDeclaredEmpty :: elType -> el_ -> elFinal
  endAttrsEndElementDeclaredEmpty _ = endAttrsEndElement (undefined :: elType)
  endAttrsEndElement _ = endAttrsEndElementDeclaredEmpty (undefined :: elType)

class AddText el_ text where
  addText :: el_ -> text -> el_ -- el_, text node

-- after this no more attributes
class EndEl elType el_ elFinal where
  endEl :: elType -> el_ -> elFinal

class XmlDoc rootElType el_ elA_ | el_ -> elA_, elA_ -> el_ where
  xmlDoc :: rootElType -> XmlIds -> el_ -> elA_

-- ========== type level implementation ==============================


class XmlIdsFromRoot rootElType where
  xmlIds :: rootElType -> XmlIds -- public, system id

class XmlDocT elst el_ doc 
  |  doc elst -> el_ where
  xmlDocT :: PT elst el_ -> doc

instance ( XmlDoc rootElType el2 doc
        , XmlIdsFromRoot rootElType
        , EndElT (Valid rootElType) el st el2
                            ) => XmlDocT (Valid rootElType) el doc where
  xmlDocT e = 
        let PT el = endElT e
        in xmlDoc (undefined :: rootElType)
               (xmlIds (undefined :: rootElType))
               el
               

data PT a b = PT b -- phantom type containing state a and the result b

-- ============ create an ElT ==========================================

-- instances of InitialState will be generated by TH 
class InitialState elType initialState
    | elType -> initialState
class (
  InitialState elType initialState
  , CreateEl elType el
  ) => CreateElT elType initialState el where
  createElT :: elType -> PT initialState el
  createElT _ = PT $ createEl (undefined :: elType)

class AttrOk elType attr
class AddAttrT attrType attrValue st el_ st2 el2_
      | st attrType -> st2
      , el2_ -> el_, el_ -> el2_
  where
  addAttrT :: PT st el_ -> attrType -> attrValue -> PT st2 el2_

instance (
       AddAttribute el el2 attrType attrValue
      , AttrOk elType attrType
#ifndef WEAK_VALIDATION
      , -- remove attribute from required list
        HMemberM (A attrType) req mreq
      , HFromMaybe mreq req req'
      , -- no attribute may be added twice
        HMember (A attrType) added rd
      , CheckDuplicateAttribute elType attrType rd
#endif
      , DetermineEl (AS req  added) el
                    (AS HNil (HCons (A attrType) added)) el2
      ) => AddAttrT attrType attrValue
                    (NYV (Element elType (AS req  added) st HFalse)) el
                    (NYV (Element elType (AS req' (HCons (A attrType) added)) st HFalse)) el2


  where
  addAttrT (PT t) _ v = PT (addAttribute t (undefined :: attrType) v)
     -- fail nicer error messages
instance ( YouCantAddAttributesAfterAddingContentTo elType
      ) => AddAttrT attrType attrValue (NYV (Element elType stA st HTrue)) el st2 el2
  where addAttrT = undefined -- shut up warning


#ifndef WEAK_VALIDATION
class HFromMaybe mb b r | mb b -> r
instance  HFromMaybe HNothing b b
instance  HFromMaybe (HJust a) b a
class CheckDuplicateAttribute elType attrType hbool
instance CheckDuplicateAttribute elType attrType HFalse
instance ( DuplicateAttribute elType attrType
         ) => CheckDuplicateAttribute elType attrType HTrue
#endif

-- ========== ending attributes ====================================== 
class EndAttrsT est el est2 el2 
    | est est2 el2 -> el
    , est -> est2
  where
  endAttrsT :: (PT est el) -> (PT est2 el2)

instance (
    EndAttrs el el2
    , StEndAttrs elType stA
    , DetermineEl (NYV (Element elType stA     st HFalse)) el 
                  (NYV (Element elType AttrsOk st HFalse)) el2
    ) => EndAttrsT (NYV (Element elType stA     st HFalse)) el
                   (NYV (Element elType AttrsOk st HFalse)) el2
  where
  endAttrsT (PT t) = PT $ endAttrs t

-- no operation instance used in AddElT 
instance (
    ) => EndAttrsT (NYV (Element elType AttrsOk st hchs)) el
                   (NYV (Element elType AttrsOk st hchs)) el
  where
  endAttrsT  = id
-- ========== adding sub elements (tags) attrs ok =============================
class AddElT est el_ 
             estc elc_
             est2 el2_
    | est estc -> est2
    , est est2 estc el2_ -> el_
    , est est2 estc el2_ -> elc_
    where
  addElT :: PT est el_ -> PT estc elc_ -> PT est2 el2_

instance (
        EndElT cest elc 
              (Valid celType) elc2

      , EndAttrsT (NYV (Element elType stA st hchs)) el
                  (NYV (Element elType AttrsOk st hchs)) el2

      , DetermineEl (NYV (Element elType stA st hchs)) el
                    (NYV (Element elType AttrsOk st hchs)) el2

      , DetermineElAddEl 
              (NYV (Element elType AttrsOk st hchs)) el2
              cest2 elc2
              (NYV (Element elType AttrsOk st2 HTrue)) el3
      , AddEl el2 el3 elc2
#ifdef WEAK_VALIDATION
      , TypesEq st st2
      , ChildOk elType celType
#else
      , Consume st (Elem celType) st2
#endif

  ) => AddElT (NYV (Element elType stA st hchs)) el
              cest elc
              (NYV (Element elType AttrsOk st2 HTrue)) el3
              where
    addElT e c = 
      let (PT e') = endAttrsT e
          (PT c') = endElT c
      in PT $ addEl (e' :: el2) (c' :: elc2)
  

-- ========== adding sub elements (text) =============================
class AddTextT el_ el2_ text elst elst2 | el_ -> el2_, el2_ -> el_, elst -> elst2 where
  addTextT :: PT elst el_ -> text -> PT elst2 el2_
-- first child
instance (
    EndAttrs el el2
  , StEndAttrs elType stA
  , AddText el2 text
#ifdef WEAK_VALIDATION
  , TypesEq st st'
  , ChildOk elType PCDATA
#else
  , Consume st PCDATA st'
#endif
  ) => AddTextT el el2 text (NYV (Element elType stA st  HFalse))
                            (NYV (Element elType stA st' HTrue))
  where
  addTextT (PT t) text = PT $ addText (endAttrs t) text
-- not first child (TODO: merge both instances)
instance (
    AddText el text
#ifdef WEAK_VALIDATION
  , TypesEq st st'
  , ChildOk elType PCDATA
#else
  , Consume st PCDATA st'
#endif
  ) => AddTextT el el text (NYV (Element elType stA st  HTrue))
                           (NYV (Element elType stA st' HTrue))
  where
  addTextT (PT t) text = PT $ addText t text

-- ========== final element check ====================================
class EndElT st el_ st2 el2_
      | st -> st2
      , st st2 el2_ -> el_
    where
    endElT :: (PT st el_)
           -> (PT st2 el2_)
-- end element with childs
#ifdef WEAK_VALIDATION
instance ( EndEl elType el el2
         , ElEndable elType st
         , DetermineEl (NYV (Element elType stA WeakValidation HTrue)) el
                             (Valid elType) el2
         ) => EndElT (NYV (Element elType stA WeakValidation HTrue)) el
                     (Valid elType) el2
  where endElT (PT el_) = PT (endEl (undefined :: elType)  el_)
#else
instance ( EndEl elType el el2
         , ElEndable elType st
         , DetermineEl (NYV (Element elType stA st HTrue)) el
                             (Valid elType) el2
         ) => EndElT (NYV (Element elType stA st HTrue)) el
                     (Valid elType) el2
  where endElT (PT el_) = PT (endEl (undefined :: elType)  el_)
#endif
-- end elements without childs declared EMPTY
instance ( EndAttrsEndElement elType el el2
         , StEndAttrs elType stA
         , DetermineEl (NYV (Element elType stA EMPTY HFalse)) el
                       (Valid elType) el2
         ) => EndElT (NYV (Element elType stA EMPTY HFalse)) el
                     (Valid elType) el2
  where endElT (PT el_) = PT (endAttrsEndElementDeclaredEmpty (undefined :: elType) el_)
-- end elements without childs not declared EMPTY
#ifdef WEAK_VALIDATION
instance (
          EndAttrsEndElement elType el el2
         , ElEndable elType st
         , DetermineEl (NYV (Element elType stA st HFalse)) el
                       (Valid elType) el2
         ) => EndElT (NYV (Element elType stA WeakValidation HFalse)) el
                     (Valid elType) el2
  where endElT (PT el) = PT $ endAttrsEndElement (undefined :: elType) el
#else
instance ( EndAttrsEndElement elType el el2
         , StEndAttrs elType stA
         , ElEndable elType st
         , DetermineEl (NYV (Element elType stA st HFalse)) el
                       (Valid elType) el2
         ) => EndElT (NYV (Element elType stA st HFalse)) el
                     (Valid elType) el2
  where endElT (PT el_) = PT (endAttrsEndElement (undefined :: elType) el_)
#endif

-- ========== the uglier part, the validation by transforming states

-- ========== elements and subelements / consume classes =============

-- element content accept stuff
data Element elType stAttributes elementsState hasChilds
data AS req added -- attribute state
data AttrsOk      -- is replaced by this after the first element has been added 
-- elType: the element type_T
-- hasChilds: HTrue or HFalse, is necessary to to know wether empty tags <br/> can be used

-- ========== allowed to end element ? ===============================
class ElEndable elType a
class StEndAttrs elType elst
#ifdef WEAK_VALIDATION
instance ElEndable elType all
instance  StEndAttrs elType all
#else
instance ElEndable elType C
instance ElEndable elType EMPTY
instance ElEndable elType ANY
instance ElEndable elType PCDATA
  -- fail nicer error messages
instance ( MoreElementsExpected elType (Elem a)) =>  ElEndable elType (Elem a)

instance  StEndAttrs elType (AS HNil added)
instance StEndAttrs elType AttrsOk -- already ok 
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
#ifdef WEAK_VALIDATION
-- only check wether this child is allowed, but not the order 
-- instances are generated in TH.hs
class ChildOk elType child
#else
data C      -- consumed, no element left
data F a    --

class Consume st el_ r | st el_ -> r -- result is on of C,CS,R,F

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

#ifdef WEAK_VALIDATION
data WeakValidation -- used instead of the element state 
class TypesEq a b | a -> b, b -> a
instance TypesEq a a
#endif

---  ========= type level equality helper =============================
-- Thanks Oleg Kiselyov for helping me here
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
