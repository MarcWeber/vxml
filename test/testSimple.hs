{-# LANGUAGE ScopedTypeVariables,  NoImplicitPrelude, FlexibleInstances,  NoMonomorphismRestriction,
 UndecidableInstances,  FlexibleContexts,  EmptyDataDecls, TemplateHaskell,
 StandaloneDeriving, TypeSynonymInstances, MultiParamTypeClasses, FunctionalDependencies
 #-}
#if(__GLASGOW_HASKELL__ > 608)
{-# LANGUAGE ScopedTypeVariables #-}
#else
--  ? why doesn't ghc-6.8.2 recognize this? 
{-# LANGUAGE PatternSignatures #-}
#endif


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

$( do dtdToTypes Nothing
                 "dtds/test_simple.dtd"
                 (XmlIds (Just "custom-simple-test")
                         (Nothing) )
 )




fStr :: (a,String) -> (a,String)
fStr = id

main = do

  let h s = putStrLn "" >> putStrLn ("== " ++ s)
      t s b = do putStrLn ""
                 putStrLn s
                 putStrLn $ "should be : " ++ b
      tS = putStrLn .      ("      xml : "  ++) . fromPT
      tDo f = do
              -- let t@(a, b :: String) = f -- String selects result type. (see instances directory)
              let t@(a, b) = fStr f
              putStrLn $    "      xml : " ++ b
              putStrLn $    "returned value : " ++ (show a)
      tDoM f = do
              t@(a, b) <- liftM fStr $ f
              putStrLn $    "      xml : " ++ b
              putStrLn $    "returned value (inner monad result) : " ++ (show a)
      e = vxmlreturn ()

  h "simple test ghc-6.8.2 compatible start - adding subelements piecwise by using addElT"
  t "only root"
    "<root/>"
  tS root_TT

  t "root with subelement a"
    "<root><a/></root>"
  tS $ (root_TT `addElT` a_TT)

  t "root with subelement a and b:"
    "<root><a/><b/></root>"
  tS $ (root_TT `addElT` a_TT) `addElT` b_TT

  t "root with subelement a and b, a having one, b having two attributes"
    "<root><a aAttr=\"aAttr attribute\"/><b aAttr=\"aAttr attribute\" bAtt=\"bAttr attribute\"/></root>"
  tS $ root_TT `addElT` (a_TT `aAttr_AA` "aAttr attribute")
               `addElT` (b_TT `aAttr_AA` "aAttr attribute" `bAttr_AA` "bAttr attribute")


  h "The same using monadic like functions"
  t "only root"
    "<root/>"
  tDo $ runRoot e
  
  t "root with subelement a"
    "<root><a/></root>"
  tDo $ runRoot $ a e

  t "root with subelement a and b:"
    "<root><a/><b/></root>"
  tDo $ runRoot $ vxmlgtgt  (a e) (b e)
  t "root with subelement a and b, a having one, b having two attributes"
    "<root><a aAttr=\"aAttr attribute\"/><b aAttr=\"aAttr attribute\" bAttr=\"bAttr attribute\"/></root>"
  tDo $ runRoot $ vxmlgtgt 
      (a (aAttr "aAttr attribute"))
      (b (vxmlgtgt (aAttr "aAttr attribute") (bAttr "bAttr attribute")) )

{- broken
  t "of course you can add a doctype as well" "with doctype"
  tDo $ runRootDoc  e
  tDo $ runRootDoc $ vxmlgtgt 
          (a (aAttr "aAttr attribute"))
          (b (vxmlgtgt (aAttr "aAttr attribute") (bAttr "bAttr attribute")) )
-}

#if (__GLASGOW_HASKELL__ > 608)
#include "vxmldos.h"

  h "using do like notation (the authors prefered way to write XML)"
  tDo $ runRoot $ vdo
    a e
    b e
  tDo $ runRoot $ vdo
    a $ aAttr "aAttr attribute"
    b $ vdo
      aAttr "aAttr attribute"
      bAttr "bAttr attribute"
  t "do like notation"
    "TODO"
  tDo $ runRoot $ vdo
          a e
          return "hi"
  t "do like notation with inner monad printing \"hello\\nworld\" returning progName"
    "<root/> should print hello\nvxml world!"
  -- putStrLn $ fromPT $ snd $ runRootInt e

  tDoM $ runRootT $ vdo
            vxmllift $ putStrLn "hello" Prelude.>> putStrLn "vxml world!"
            vxmllift $ liftM ("my progname is " ++) getProgName

  let testDoc = vdo
            vxmllift $ pdo
              putStrLn "hello"
              putStrLn "vxml world!"
            b e
            vxmllift $ liftM ("my progname is " ++) getProgName

  -- doctype currently broken 
  tDoM $ runRootT $ testDoc
  -- the same without doctype (there is a run<elem>[T] function for each tag
  -- which you can use to generate only subparts of an xml document. This is often used
  -- in Javascript where the innerHtml of a DOM object is set
  tDoM $ runRootT $ testDoc

  -- TODO: how to solve this?
  print $ fStr $ runRoot $ vdo
          -- forceElements
          (c e)
          vxmlSeqPlus_ ((d e), replicate 9 (d e))
          -- vxmlMapSeqPlus_ (\n -> d e ) [1..10]
#endif
  print "\nsimple test end"
