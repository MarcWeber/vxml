{-# OPTIONS_GHC -XTemplateHaskell -XStandaloneDeriving -XTypeSynonymInstances -XMultiParamTypeClasses -XFunctionalDependencies #-}
{-# LANGUAGE OverlappingInstances, UndecidableInstances,  FlexibleContexts,  FlexibleContexts,  FlexibleInstances,  EmptyDataDecls #-}
module Main where
import Data.HList
import Language.Haskell.TH.All
import Language.Haskell.TH.FixedPpr
import Control.Monad
import Directory
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

-- deriving instance Show DocTypeDecl
-- deriving instance Show MarkupDecl

-- deriving instance Show ElementDecl
-- deriving instance Show AttListDecl
-- deriving instance Show EntityDecl
-- deriving instance Show NotationDecl
-- deriving instance Show Misc

-- deriving instance Show PublicID
-- deriving instance Show GEDecl
-- deriving instance Show PEDecl
-- deriving instance Show AttDef
-- deriving instance Show ContentSpec

-- deriving instance Show Mixed
-- deriving instance Show DefaultDecl
-- deriving instance Show AttType
-- deriving instance Show EntityDef

-- deriving instance Show NDataDecl
-- deriving instance Show TokenizedType
-- deriving instance Show EnumeratedType
-- deriving instance Show FIXED


withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory path f = do
    cur <- getCurrentDirectory
    setCurrentDirectory path
    finally f (setCurrentDirectory cur)

-- hack, change current working directory cause HaXmL doesn't find ENTITY refs 
readDTD file = do
                print =<< getCurrentDirectory
                liftM (dtdParse' file) $ readFile file

-- HaXmL only returns a loose list of element and attr list declarations 
-- this functions ties them.

$( dtdToTypes "dtds/test_simple.dtd" 
              (XmlIds (Just "-//W3C//DTD XHTML 1.0 Strict//EN") 
                      (Just "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd") ) 
 )



dummy :: ( Consume (Seq (Or  (Elem  A_T) (Elem B_T)) 
                        (Star (Or  (Elem  A_T) (Elem B_T)))
                    )
                    B_T r 
         , Consume r A_T r'
         , Consume r' A_T r2
        ) => r2
dummy = undefined


--  )
main = do
  -- [arg] <- getArgs
  let arg = "/pr/haskell/vxml/dtds/xhtml1-20020801/DTD/xhtml1-strict.dtd"
  let arg = "/pr/haskell/vxml/dtds/test.dtd"
  -- readdtd arg >>= \r -> case r of
    -- left a -> putstrln a
    -- right (just (dtd name mbextid decls))  -> mapm_ print $ zipelements decls
  putStrLn $ xmlDocT $ addAttrT (addTextT (addElT (addElT root b) a) "abc")
       

  -- putStrLn $ fromPT $ endElT $  createElT (undefined :: Root_T)
  print "end"



