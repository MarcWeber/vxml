{-# LANGUAGE UndecidableInstances, FlexibleContexts,  MultiParamTypeClasses,  FlexibleInstances,  EmptyDataDecls,  TemplateHaskell #-}
module TestXHTML where
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

-- only compile all the generated code once in this module 

$( 
      dtdToTypes "dtds/xhtml1-20020801/DTD/xhtml1-strict_onefile.dtd" 
              (XmlIds (Just "-//W3C//DTD XHTML 1.0 Strict//EN") 
                      (Just "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd") ) 
 )

