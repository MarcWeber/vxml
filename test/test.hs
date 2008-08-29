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

-- import TestXHTML

$( 
      dtdToTypes "dtds/xhtml1-20020801/DTD/xhtml1-strict_onefile.dtd" 
      -- dtdToTypes "dtds/test_simple.dtd" 
              (XmlIds (Just "-//W3C//DTD XHTML 1.0 Strict//EN") 
                      (Just "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd") ) 
 )

(<<) = addElT
(<<<) = addTextT

main = do

  putStrLn $ xmlDocT $
    ((html << (head << (title <<< "hw")))
          <<  (body << (div <<< "test" )) )
  print "end"


