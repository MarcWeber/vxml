{-# LANGUAGE PatternSignatures #-}
{- to be run from repo dir 
-}
module Main where
import Test.BenchPress
import Data.Char
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
import TestUtil (haskellFile, compileHaskellFile, runProcess', delFile)

import XmlToQ () -- not needed here, force type checking

test tmp repoDir = do
  let tmP = (tmp </>)
      cp a b = copyFile (repoDir </> a) (tmp </> b)
      dtd = tmP "dtd.dtd"
      xmlF = tmP "xml.xml"
      dtdF = tmP "dtd.dtd"
      mainHs = tmP "Main.hs"
      exe = tmP "main"
      importHs = tmP "Import.hs"

  -- setup action 
  let setup = do
          -- DTD 
          copyFile (repoDir </> "dtds/xhtml1-20020801/DTD/xhtml1-strict_onefile.dtd") dtdF
          -- Main.hs 
          writeFile mainHs $ haskellFile repoDir Nothing ["Import", "XmlToQ"] [
             "main = $(xmlToQ \"Import.\" \"" ++ xmlF ++ "\")" 
            ]
          -- Import.hs 
          writeFile importHs $ haskellFile repoDir (Just "Import") [] [
              "$( dtdToTypes \"" ++ dtdF ++ "\" (XmlIds (Nothing) (Just \"" ++ dtdF ++ "\") ) )"
            ]
         -- test action
  let test repeat = do
        -- xml.xml 
        writeFile xmlF $  unlines $  ["<!DOCTYPE html SYSTEM \"" ++ dtdF ++ "\">" ]
                          ++ xmlhead ++ ( concat $ replicate repeat xmlbody) ++ [ "</body></html>" ]
        writeFile (tmP "count") $ show repeat
        mapM_ delFile $ map tmP ["Main.o", "main", "Main.hi"]
        ec <- compileHaskellFile repoDir mainHs exe
        case ec of 
          ExitFailure ef -> error "exit failure ghc"
          ExitSuccess -> return ()

  let runTest repeat = do
      (cpuStats, timeStats) <- benchmark 1 setup (const $ return ()) (const $ test repeat)

      -- cpu time stats don't work on my machine
      -- putStrLn "c\npu"
      -- printDetailedStats cpuStats
      putStrLn "\ntimeStats"
      -- printDetailedStats timeStats
      -- putStrLn $ "measured time (cpu ): " ++ (show repeat) ++ " " ++ (show $ mean cpuStats)
      putStrLn $ "measured time (wall): " ++ (show repeat) ++ " " ++ (show $ mean timeStats)
  mapM_ runTest [1..100]


main = do
  tmp <- liftM (</> "vxml-benchpress")  getTemporaryDirectory
  createDirectoryIfMissing True tmp
  repoDir <- getCurrentDirectory
  Main.test tmp repoDir

xmlhead = 
        [ "<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en-US\" xml:lang=\"en-US\">"
        , "<head>"
        , "  <title>Example 6 - XHTML 1.0 Strict as application/xhtml+xml</title>"
        , "  <meta http-equiv=\"Content-Type\" content=\"application/xhtml+xml; charset=utf-8\" />"
        , "  <link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />"
        , "</head>"
        , "<body>"
        ]

xmlbody = 
        [ "  <h1>Example 6 - XHTML 1.0 Strict as application/xhtml+xml</h1>"
        , "  <p>"
        , "   This document is valid XHTML 1.0 Strict served as"
        , "   <code>application/xhtml+xml</code>."
        , "  </p>"
        , " "
        , "  <p>"
        , "  This document references CSS rules contained in an external"
        , "  stylesheet via <code>link</code>."
        , "  </p>"
        , " "
        , "  <p>"
        , "  Note how the CSS rules for the background are applied in Netscape 7.x,"
        , "  Mozilla, Opera 7 but that Internet Explorer can not display the page at all."
        , "  </p>"
        , " "
        , "  <p>"
        , "    <a href=\"http://validator.w3.org/check/referer\"><img"
        , "        src=\"http://www.w3.org/Icons/valid-xhtml10\""
        , "        alt=\"Valid XHTML 1.0!\" height=\"31\" width=\"88\" /></a>"
        , "  </p>"
        ]
