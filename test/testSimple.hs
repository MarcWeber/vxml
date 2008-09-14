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
      t s = putStrLn "" >> putStrLn s
      tS = putStrLn . fromPT
      tDo f = do
              -- let t@(a, b :: String) = f -- String selects result type. (see instances directory)
              let t@(a, b) = fStr f
              putStrLn $ "returned value : " ++ (show a)
              putStrLn $ "xml : " ++ b
      tDoM f = do
              t@(a, b) <- liftM fStr $ f
              putStrLn $ "returned value (inner monad result) : " ++ (show a)
              putStrLn $ "xml : " ++ b
      e = vxmlreturn ()
  h "simple test ghc-6.8.2 compatible start"

  t "only root"
  tS root_TT

  t "root with subelement a"
  tS $ root_TT `addElT` a_TT

  t "root with subelement a and b"
  tS $ root_TT `addElT` a_TT `addElT` b_TT

  t "root with subelement a and b, a having one, b having two attributes "
  tS $ root_TT `addElT` (a_TT `aAttr_AA` "aAttr attribute")
               `addElT` (b_TT `aAttr_AA` "aAttr attribute" `bAttr_AA` "bAttr attribute")

  t "simple vxml* function test > ghc-6.8.2 do notation is preferred"
  tDo $ runRootDoc e
  tDo $ runRootDoc $ a e
  tDo $ runRootDoc $ (a e) `vxmlgtgt` (b e)
  tDo $ runRootDoc $ a (aAttr "aAttr attribute")
                      `vxmlgtgt`
                      b (aAttr "aAttr attribute" `vxmlgtgt` bAttr "bAttr attribute")


#if (__GLASGOW_HASKELL__ > 608)
#include "vxmldos.h"

  -- using the vxmlgtgt, vxmlbind, vxmlreturn does work in ghc-6.8.2 as well. But its not as convinient

  h "simple test start > ghc-6.8.2 compatible"
  tDo $ runRoot $ vdo
    a e
    b e
  tDo $ runRoot $ vdo
    a $ aAttr "aAttr attribute"
    b $ vdo
      aAttr "aAttr attribute"
      bAttr "bAttr attribute"
  t "do like notation"
  tDo $ runRootDoc $ vdo
          a e
          return "hi"
  t "do like notation with inner monad printing \"hello\\nworld\" returning progName"
  tDoM $ runRootDocT $ vdo
            vxmllift $ putStrLn "hello" Prelude.>> putStrLn "vxml world!"
            vxmllift $ liftM ("my progname is " ++) getProgName
  let testDoc = vdo
            vxmllift $ pdo
              putStrLn "hello"
              putStrLn "vxml world!"
            b e
            vxmllift $ liftM ("my progname is " ++) getProgName

  tDoM $ runRootDocT $ testDoc
  -- the same without doctype (there is a run<elem>[T] function for each tag
  -- which you can use to generate only subparts of an xml document. This is often used
  -- in Javascript where the innerHtml of a DOM object is set
  tDoM $ runRootT $ testDoc
#endif
  print "simple test end"
