-- helper module. creates TH code which generates code generating the xml of the passed file 
-- used in run-testcases.hs

module XmlToQ where
import Text.XML.HaXml.Verbatim
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Types
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import System.IO
import Control.Monad
import Data.Char

import Text.XML.Validated.TH
import Text.XML.Validated.Util


xmlToQ :: FilePath -> ExpQ
xmlToQ file =  do
    (Document prolog symTab element misc) <- runIO $ liftM (xmlParse file) $ readFile file
    res <- app "putStrLn" $ app "xml" $ elemToQ element
    runIO $ do
     hPutStrLn stdout "-- ============= generated code ================"
     hPutStrLn stdout $ pprint res
     hPutStrLn stdout $ "-- ============= end generated code ============"
     hFlush stdout

    return res
  where 
    app a b = appE (varE (mkName a)) b
    elemToQ (Elem en atts content) =
      -- attributes 
      let elemWithAtts = foldl (\a (an, attV) -> appEn ( varE $ mkName $ fstLower $ attrHaskell an ) 
                                                  [a, ( stringE $ show attV )] ) (varE $ mkName en)  atts
      -- content 
      in foldl (\el c -> addContent c el) elemWithAtts content
    -- addContent :: Content Posn -> ExpQ ->ExpQ 
    addContent (CElem elem _) el = appEn (varE $ mkName "addElT") [ el, elemToQ elem ]
               -- TODO handle bool 
    addContent (CString bool charData _) el = if (all isSpace charData) then el 
                                              else appEn (varE $ mkName "addTextT") [ el, stringE charData ]
               -- <foo>\n<bar/> will result addTextT "\n"..
               -- this is will be an error according to some test dtds, so only
               -- add text containing non whitespace characters
    addContent (CRef reference _) el = error "not yet implemented: misc"
    addContent (CMisc misc _) el = error "not yet implemented: misc"

