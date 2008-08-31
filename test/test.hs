{-# OPTIONS_GHC -fcontext-stack=500 #-}
{-# OPTIONS_GHC -XTemplateHaskell -XStandaloneDeriving -XTypeSynonymInstances -XMultiParamTypeClasses -XFunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances,  FlexibleInstances,  FlexibleInstances,  NoMonomorphismRestriction,  UndecidableInstances,  FlexibleContexts,  EmptyDataDecls #-}

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
import Text.XML.Validated.Types
import Text.XML.Validated.Instance.String
import Text.XML.HaXml.Types

import Text.XML.HaXml.Types
import Text.XML.Validated.TH

import TestXHTML

main = do

  putStrLn $ xml $
    ((html << (head << (title <<< "hw")))
          <<  (((script `type_A` "text/javascript") <<< "test" )) )
  print "end"


