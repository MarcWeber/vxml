{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances,  NoMonomorphismRestriction,  UndecidableInstances,  FlexibleContexts,  EmptyDataDecls #-}
{-# OPTIONS_GHC -XTemplateHaskell -XStandaloneDeriving -XTypeSynonymInstances -XMultiParamTypeClasses -XFunctionalDependencies #-}
{-# OPTIONS_GHC -fcontext-stack=500 #-}
#if (__GLASGOW_HASKELL__ > 608)
  {-# LANGUAGE ScopedTypeVariables #-}
#else
  {-# LANGUAGE PatternSignatures #-}
#endif

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

import Text.XML.Validated.TH
import Data.HList

import TestXHTML as X

fStr :: (a,String) -> (a,String)
fStr = Prelude.id

main = do
  let tDo f = do
              -- let t@(a, b :: String) = f -- String selects result type. (see instances directory)
              let t@(a, b) = fStr f
              putStrLn $ "returned value : " ++ (show a)
              putStrLn $ "xml : " ++ b
      tDoM f = do
              t@(a, b) <- liftM fStr $ f
              putStrLn $ "returned value (inner monad result) : " ++ (show a)
              putStrLn $ "xml : " ++ b

      e = vxmlreturn ()

#if (__GLASGOW_HASKELL__ > 608)
#include "vxmldos.h"
  tDo $ runHtmlDoc $ vdo
    head $ title $ text "text"
    body $ vdo
      script $ X.type "text/javascript" >> text "document.writeln('hi');"
      h2 $ text "That's a headline, hello and how do you do?"
      -- br e   eg a <br/> is not allowed here
      div $ vdo
        onclick "alert('clicked');"
        styleA "color:#F79"
        text "text within the div"
      div e
      return "That's nice, isn't it?"

  tDoM $ runHtmlDocT $ vdo
    xmlns "http://www.w3.org/1999/xhtml"
    lang "en-US"
    -- xml:lang "en-US"
    head $ title $ text "minimal"
    body $ do
      args <- lift $ getArgs
      h1 $ text "minimal"
      div $ text $ "args passed to this program: " ++ (show args)

#endif

  print "end"
