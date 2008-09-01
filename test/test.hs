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

(#) = flip (.)

main = do

  -- you probably want to use names without _T and _A if possible 
  putStrLn $ xml $
    ((html_T << ( head_T << (title_T <<< "hw")
                         << (link_T `rel_A` "stylesheet" `type_A` "text/css" `href_A` "style.css")
              ))
          <<  ( body_T << ((script_T `type_A` "text/javascript") <<< "document.writeln('hi');" )
                       << (div_T `onclick_A` "alert('clicked');" `style_A` "color:#F79"
                              <<< "text within the div"
                        )
              ) )
  putStrLn "alternative user interface"
  putStrLn $ xml $
    ( headC ( (titleC (<<< "hw"))
            # (linkC (rel_AF "stylesheet" # type_AF "text/css" # href_AF "style.css" ) )
            )
    # bodyC ( scriptC ( type_AF "text/javascript" # text "document.writeln('hi');" )
            # divC ( onclick_AF "alert('clicked')" # style_AF "color:#F79"
                  # text "text within the div" )
            )
    ) html_T

  print "end"


