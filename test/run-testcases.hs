{-# LANGUAGE PatternSignatures #-}
{- thisprogram dir n runs n of the *.txt file tests given in dir.
   a tesfile looks like this:
dtd:
<dtd spec>
valid:
<valid xml code>
invalid:
<invalid xml code, should be rejected by type errors by this lib>

valid: and invalid: xml code blocks can be given multiple times

The working directory of this application must be the repository dirctory so
that some source files are found by the test application.

It will write
  xml.xml
  dtd.dtd
  test.hs
  xmllint.log
  ghc.log
into getTemporaryDirectory

-}

 

module Main where
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

data TestFile = TestFile {
  file :: FilePath
  , dtd :: String
  , valid :: [ String ]
  , invalid :: [ String ]
  } deriving (Show)

readTestFile :: FilePath -> IO TestFile
readTestFile f = do
  fContents <- readFile f
  return $ split f $ lines fContents
  where 
    split f ("dtd:":lines) = 
        let (dtd, rest) = myBreak lines
            (valid, invalid) = splitRest rest
        in TestFile f (unlines dtd) valid 
#ifdef DoValidate
                    invalid
#else
                    [] -- does QuickCheck support skipping invalid tests?
#endif
    splitRest :: [ String ] -> ([String], [String])
    splitRest ("valid:":lines) = 
        let (valid, rest) = myBreak lines
            (valid', invalid) = splitRest rest
        in ((unlines valid):valid', invalid)
    splitRest ("invalid:":lines) = 
        let (invalid, rest) = myBreak lines
            (valid, invalid') = splitRest rest
        in (valid, (unlines invalid) :  invalid')
    splitRest [] = ([], [])
    splitRest err = error $ f ++ ": error in splitRest" ++ (show err)
    myBreak = break (`elem` ["valid:", "invalid:"])


-- testCases :: FilePath -> FilePath -> FilePath -> FilePath -> TestFile -> Test
testCases hs exeF xmlF dtdF (TestFile f dtd valid invalid) =
  let tests = zip (cycle [dtd]) $ zip (cycle [True]) valid ++ zip (cycle [False]) invalid
      ecToBool ExitSuccess = True
      ecToBool _ = False
      testAction :: (Int, ( String, (Bool, String))) -> Test
      testAction (n, (dtdContent, (valid, xmlContent))) = TestLabel ({- (show f) ++-} (show n)) $ TestCase $ assert $ do
        cwd <- getCurrentDirectory
        let rootElement = ( 
                        takeWhile (not . isSpace)
                      . dropWhile isSpace
                      . drop (length "<!ELEMENT ")
                      . head
                      . dropWhile (not . isPrefixOf "<!ELEMENT ") . lines) dtd
        writeFile xmlF ( "<!DOCTYPE " ++ rootElement ++ " SYSTEM \"" ++ dtdF ++ "\">" ++ xmlContent)
        writeFile dtdF $ "<!-- " ++cwd </> f ++" --> \n" ++ dtd

        -- try xmllint 
        xmllint <- findExecutable "xmllint"
        case xmllint of
          Nothing -> putStrLn "warning, no xmllint found !"
          Just p -> do 
               h <- runProcess' (dropFileName hs) "xmllint" p ["--valid","--loaddtd", "--noout", "--load-trace", xmlF ]
                        
               ec <- waitForProcess h
               (Just vim) <- findExecutable "vim"
               {- run vim to edit files which don't pass xmllint the expected way
               when (ecToBool ec /= valid) $ do 
                  p <- runProcess vim [ "-p", xmlF, dtdF, f, "-c", ":w|cope" ] Nothing Nothing Nothing Nothing Nothing
                  waitForProcess p
                  return ()
               -}
               when (ecToBool ec /= valid) $ fail $ " valid = " ++ (show valid) ++ " expected, but >> xmllint << exited with " ++ (show ec)

 
        putStrLn $ "writing to " ++ hs
        -- try this library, there is already a dependency on HaXml, so why not
        -- use it to parse the xml code again?
        writeFile hs $ haskellFile cwd Nothing ["XmlToQ"] [
              "-- based on " ++ (cwd </> f)
            , "$( dtdToTypes \"" ++ dtdF ++ "\" (XmlIds (Nothing) (Just \"" ++ dtdF ++ "\") ) )"
            , "main = $(xmlToQ  \"\" \"" ++ xmlF ++ "\")" 
            ]
        ec <- compileHaskellFile cwd hs exeF
        when (ecToBool ec /= valid) $ fail $ " valid = " ++ (show valid) ++ " expected, but ghc exited with " ++ (show ec)
              -- TODO parse the generated result file and compare against input 
  in f ~: TestList $ map testAction $ zip [1..] tests


main = do
  (testcasesDir: pref') <- getArgs
  let pref = case pref' of
                [] -> ""
                [a] -> a
  list <- liftM ( map (testcasesDir </>)
                . filter (".txt" `isSuffixOf`) ) $ getDirectoryContents testcasesDir
  tmpDir <- getTemporaryDirectory
  let xml = tmpDir </> "xml.xml"
  let dtd = tmpDir </> "dtd.dtd"
  let hs  = tmpDir </> "test.hs"
  let exe = tmpDir </> "test"
  putStrLn $ unlines [ 
        "you'll have to remove the testfiles "
        , xml ++ ", " ++ dtd
        , "manually after this test" ]


  (testFiles :: [TestFile] ) <- mapM readTestFile $ filter (isPrefixOf pref . takeFileName) list
  runTestTT $ TestList $ map (testCases hs exe xml dtd) testFiles
