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
module TestXHTML where
import Prelude ((++),(.))
import Language.Haskell.TH
import Control.Monad
import Directory
import Data.HList
import Control.Exception
import System.FilePath
import System.IO
import System.Environment
import Text.XML.Validated.Types
import Text.XML.Validated.Instance.String
import Text.XML.HaXml.Parse
import qualified Data.Map as M
import Data.Maybe
import Text.XML.HaXml.Types
import Text.XML.Validated.TH
import Text.XML.Validated.Util (fstLower, fstUpper)

-- only compile all the generated code once in this module 

$( do
      -- you probably want to use the daufault NameGenerator instead.
      -- using a custom one here only to assure it works
      dtdToTypes (Just intelligentNameGenerator)
                 "dtds/xhtml1-20020801/DTD/xhtml1-strict_onefile.dtd" 
                 (XmlIds (Just "-//W3C//DTD XHTML 1.0 Strict//EN") 
                         (Just "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd") ) 
 )
