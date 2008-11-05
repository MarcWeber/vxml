module TestUtil where
import Data.Char
import Data.Maybe
import Debug.Trace
import Data.List
import System.Exit
import System.Environment
import System.Process
import Control.Monad
import System.Directory
import System.IO
import System.FilePath
import Test.HUnit


runProcess' dir name app args  = do
        out <- openFile ( dir </> name ++ ".log" ) WriteMode
        p <- runProcess app args (Just dir) Nothing Nothing (Just out) (Just out)
        waitForProcess p
        hClose out
        return p

extraFlags = tail [ ""
#ifdef DoValidate
                  , "-DDoValidate" , "-XOverlappingInstances", "-DPRINT_GENERATED_CODE"
#endif
                  ]


haskellFile repoDir moduleName extraImports contents = 
        let flags = [ "-i" ++ (repoDir </>"src")
                    , "-i" ++ (repoDir </>"test"), "-cpp"]
                    ++ extraFlags
        in unlines (
          [ "-- packages: HaXml,template-haskell,containers,directory,mtl,HList,filepath"]
          ++ ( if (isNothing moduleName) 
               then [ "-- ghc-options: -XOverlappingInstances, -cpp" ++ concatMap (", " ++ ) flags ] 
               else [] ) ++ 
          [ "{-# LANGUAGE ScopedTypeVariables,  NoImplicitPrelude, FlexibleInstances,  NoMonomorphismRestriction,"
          , "UndecidableInstances,  FlexibleContexts,  EmptyDataDecls, TemplateHaskell,"
          , "StandaloneDeriving, TypeSynonymInstances, MultiParamTypeClasses, FunctionalDependencies#-}"
          , "module " ++ (fromMaybe "Main" moduleName) ++ " where"
          , "import Language.Haskell.TH"
          , "import Control.Monad"
          , "import Directory"
          , "import Data.HList"
          , "import Data.Maybe"
          , "import Control.Exception"
          , "import System.FilePath"
          , "import System.IO"
          , "import System.Environment"
          , "import Text.XML.Validated.Types"
          , "import Text.XML.Validated.Instance.String"
          , "import Text.XML.HaXml.Parse"
          , "import qualified Data.Map as M"
          , "import Data.Maybe"
          , "import Text.XML.HaXml.Types"
          , "import Text.XML.Validated.TH" 
          ] ++ (map ("import " ++) extraImports) ++ contents
          )

-- compile haskell module
-- adding repo source dires t include path
-- piping stdout/err to a log file
compileHaskellFile repoDir hs exeF = do
        mghc <- findExecutable "ghc"
        let flags = [ "-i" ++ (repoDir </>"src")
                    , "-i" ++ (repoDir </>"test"), "-cpp"]
                    ++ extraFlags ++ [ "--make", "-o", exeF, hs ]
        delFile exeF
        case mghc of
          Nothing -> error "fatal, no ghc found to run the tests!"
          Just ghc -> do 
              putStrLn $ "   cmd:  ghc " ++ (unwords flags)
              h <- runProcess' (dropFileName hs) "ghc" ghc flags
              waitForProcess h


delFile f = do
    de <- doesFileExist f
    when de $ removeFile f
