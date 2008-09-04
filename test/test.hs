{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances,  NoMonomorphismRestriction,  UndecidableInstances,  FlexibleContexts,  EmptyDataDecls #-}
{-# OPTIONS_GHC -XTemplateHaskell -XStandaloneDeriving -XTypeSynonymInstances -XMultiParamTypeClasses -XFunctionalDependencies #-}
{-# OPTIONS_GHC -fcontext-stack=500 #-}

{- see cabal file
#ifndef TypeToNat
  {-# Language OverlappingInstances  #-}
#endif
-}

module Main where
import Prelude hiding (head,div)
import Control.Monad
import System.FilePath
import System.IO
import System.Environment
import Text.XML.Validated.Types hiding ( VXMLMonad )
import Text.XML.Validated.Instance.String
-- import Text.XML.HaXml.Types

-- import Text.XML.HaXml.Types
import Text.XML.Validated.TH
import Data.HList

-- import TestXHTML


$( do
      -- you probably want to use the daufault NameGenerator instead.
      -- using a custom one here only to assure it works
      dtdToTypes Nothing
                 "dtds/test_simple.dtd"
                 (XmlIds (Just "-//W3C//DTD XHTML 1.0 Strict//EN") 
                         (Just "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd") ) 
 )


-- custom do 
#define vdo let (>>=) = vxmlbind; (>>) = vxmlgtgt; return = vxmlreturn; lift = vxmllift in do
-- Prelude do (I've tried giving it the name do using the token concatenation 
-- d ## o to break recursion but this did only work for one define
#define pdo let (>>=) = (P.>>=); (>>) = (P.>>); return = P.return; in do
-- note that cpp doesn't like var names such as y' <- ... 

{-
type AA = (NYV (Element A_T (AS HNil HNil) EMPTY HFalse))

class A_C m elc stc3 elc3  st2 el2 st3 el3 
      | st2 stc3 -> st3, el2 elc3 -> el3 where
  aC :: m AA  elc  AA elc  stc3 elc3 a
      -> m st el  st2 el2  st3 el3 a
instance ( AddElT st2 el2 st3 el3 stc3 elc3
        , CreateEl A_T elc
        , EndAttrsEndElement Root_T el2 el
        ) => A_C VXML elc  stc3 elc3  st2 el2 st3 el3 where
  aC f = let (a, child) = runA' f
         in VXML $ \p -> (a, (`addElT` child) . p)
instance ( Monad m
        , AddElT st2 el2 st3 el3 stc3 elc3
        , CreateEl A_T elc
        , EndAttrsEndElement Root_T el2 el
        ) => A_C (VXMLT m) elc  stc3 elc3  st2 el2 st3 el3 where
  aC f = VXMLT $ \p -> do
         (a, child) <- runAT' f
         return (a, (`addElT` child) . p)

-}

{-
class AAttr_CA m st st2 st3 el el2 value 
  | el -> el2, el2 -> el where
  aAttr :: (
        ) => value -> m  st el  
                         st2 el2
                         st3 el2
                         ()
instance ( AddAttrT AAttr_A value st2 st3 el2
        ) => AAttr_CA VXML st st2 st3 el el2 value where
  aAttr v = VXML $ \p -> ((), (\el -> addAttrT el (undefined :: AAttr_A)  v) . p)
instance ( Monad m'
        , AddAttrT AAttr_A value st2 st3 el2
        ) => AAttr_CA (VXMLT m') st st2 st3 el el2 value where
  aAttr v = VXMLT $ \p -> return ((), (\el -> addAttrT el (undefined :: AAttr_A) v) . p)


class BAttr_CA m st st2 st3 el el2 value 
  |  el -> el2, el2 -> el where
  bAttr :: (
        ) => value -> m  st el  
                         st2 el2
                         st3 el2
                         ()
instance ( AddAttrT BAttr_A value st2 st3 el2
        ) => BAttr_CA VXML st st2 st3 el el2 value where
  bAttr v = VXML $ \p -> ((), (\el -> addAttrT el (undefined :: BAttr_A)  v) . p)
instance ( Monad m'
        , AddAttrT BAttr_A value st2 st3 el2
        ) => BAttr_CA (VXMLT m') st st2 st3 el el2 value where
  bAttr v = VXMLT $ \p -> return ((), (\el -> addAttrT el (undefined :: BAttr_A) v) . p)
-}


e = vxmlreturn ()
main = do
  let x = runA $ bAttr "b attr value"  `vxmlgtgt` aAttr "a attr value" `vxmlgtgt` ( VXML $ \p -> ("tes", p) )
  let aa = a $ bAttr "aAttr" `vxmlgtgt` (aAttr "bAttr" `vxmlgtgt` (VXML $ \p -> ("t", p)))
  let x = runRoot (  (b e) `vxmlgtgt` aa)
  -- -- vxmlreturn ()
  print (x :: ( String, String))
  let y = runRoot $ (a e) `vxmlgtgt` vxmlreturn "text"
  print (y :: (String, String))


  --     -T
  let aaT = a $ bAttr "aAttr" `vxmlgtgt` aAttr "bAttr" `vxmlgtgt` (vxmllift $ getProgName )
  xT <- runRootT (  (b e) `vxmlgtgt` aaT)
  print (xT :: ( String, String ))

  y <- runRootT $ vdo 
          a e
          lift $ liftM ("my progname is " ++) getProgName
  print (y :: ( String, String ))
  {-
  y2 <- runRootT $ a $ vdo
    aA "a attr value"
    lift $ liftM ("my progname is " ++) getProgName
  print (y :: ( String, String))
  print (y2 :: ( String, String))
  -}


  -- xmlWithInnerIO <- execXmlT $ do
    -- fail
    -- head $ title $ text "minimal"
    -- body $ do
      -- args <- lift $ getArgs
      -- h1 $ text "minimal"
      -- div $ text $ "args passed to this program: " ++ (show args)
  -- putStrLn $ xmlWithInnerIO

  {-
  putStrLn "final user interface, of course monadic, you would have guessed it :-)"
  le2 doc = do
    head $ do
      title $ text "vxml hello world"
      link $ rel "stylesheet" >> type "text/css" >> href "style.css"
    body $ do
      script $ type "text/javascript" >> text "document.writeln('hi');"
      br
      div $ do 
        onclick "alert('clicked');" >> style "color:#F79"
        text "text within the div"
      div -- note, because of using classes 
      div -- you don't have to add content, but you can 
      return "That's nice, isn't it?"
  putStrLn $ execXml doc
  let (xml,thatsNice) = runXml doc
  putStrLn xml
  putStrLn thatsNice

  -- of course you can stack monads as well! 
  xmlWithInnerIO <- execXmlT $ do
  xmlWithInnerIO <- execXmlT $ do
    xmlns "http://www.w3.org/1999/xhtml" >> lang "en-US" >> xml:lang "en-US"
    head $ title $ text "minimal"
    body $ do
      args <- lift $ getArgs
      h1 $ text "minimal"
      div $ text $ "args passed to this program: " ++ (show args)
  putStrLn $ runRoot $
    xmlns "http://www.w3.org/1999/xhtml" >> lang "en-US" >> xml:lang "en-US"
    head $ title $ text "minimal"
    body $ do
      args <- lift $ getArgs
      h1 $ text "minimal"
      div $ text $ "args passed to this program: " ++ (show args)
  putStrLn $ xmlWithInnerIO
  -}
  print "end"
