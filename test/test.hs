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

import TestXHTML as X

main = do

  putStrLn $ xml $
    ((html << ( X.head << (title <<< "hw")
                    << (link `rel_A` "stylesheet" `type_A` "text/css" `href_A` "style.css")
              ))
          <<  ( body << ((script `type_A` "text/javascript") <<< "document.writeln('hi');" )
                     << (div `onclick_A` "alert('clicked');" `style_A` "color:#F79"
                              <<< "text within the div"
                        )
              ) )
  print "end"


