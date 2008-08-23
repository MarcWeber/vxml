{-# LANGUAGE TemplateHaskell #-}
module Text.XML.Validated.TH where
import Data.List
import Data.Maybe
import Control.Monad
import Text.XML.HaXml.Types
import Text.XML.HaXml.Parse
import qualified Data.Map as M
import Text.XML.HaXml.ParseLazy
import Text.XML.Validated.Util
import Text.XML.Validated.Types
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import System.IO
import Text.XML.Validated.Types

-- all the template stuff 
 

zipElements :: [MarkupDecl] -> [ ( String, (Maybe ElementDecl, Maybe AttListDecl)) ]
zipElements = M.toList . M.fromListWithKey combine . catMaybes . map toMbTuple
  where toMbTuple (Element eDecl@(ElementDecl n _)) = Just (n , (Just eDecl, Nothing) )
        toMbTuple (AttList aDecl@(AttListDecl n _)) = Just (n , (Nothing, Just aDecl) )
        combine _ (Nothing, Just a) (Just e, Nothing) = (Just e, Just a)
        combine _ (Just e, Nothing) (Nothing, Just a) = (Just e, Just a)
        combine k _ _ = error $ "something went wrong: one element or attr named " 
                    ++k ++ " list has been declared twice"

instanceShow dataName value = do
  instanceD (cxt []) (appTn (conT ''Show) [conT dataName])
    [funD 'show [ clause [wildP] (normalB (litE . stringL $ value)) []] ]
-- instance typeA typeB 
instanceOfSimple :: TypeQ -> TypeQ -> DecQ
instanceOfSimple a b = 
  instanceD (cxt []) (appTn a [b]) []

attrHaskellName = mkName . (++ "_A") . fstUpper
elementHaskellName = mkName .  (++ "_T") . fstUpper

-- initial state ready for starting type based parser
elementState el EMPTY = appE 'Fin ( conEl el )
elementState el ANY = error "ANY not yet supported"
elementState el (Mixed _) = error "MIXED not yet supported"
elementState el (ContentSpec spec) = sp spec
  where sp (TagName n mod)   = addMod mod $ conEl n
        sp (Choice list mod) = addMod mod $ choice $ map sp list
        sp (Seq list mod)    = addMod mod $ seq $ map sp list
        addMod None x = x
        addMod Query x = appE (conT 'Query) x
        addMod Star x = appE (conT 'Star) x
                                              -- (x)+ is rewritten seq [x, x*]
        seq = appE (conT 'Sequence) . list
        choice = appE (conT 'Choice) . list
        addMod Plus x =  seq [x, appE (conT 'Star) x ]
        conEl = (conT . mkName . elementHaskellName)

toCode ::  ( String, (Maybe ElementDecl, Maybe AttListDecl)) -> Q [Dec]
toCode (n, (Just (ElementDecl _ content), Just (AttListDecl _ attdefList) ) ) = do
  let elDataName  =  elementHaskellName n

  sequence [


      -- | dataElement
    dataD (cxt []) elDataName [] [] []

    , -- instanceElementType
    instanceOfSimple (conT ''ElementType) (conT elDataName)

    , -- function elname for convinience 
    funD (mkName $ fstLower n) 
         [ clause [] (normalB (appEn (varE 'createElementT) [sigE (varE 'undefined) (conT elDataName)] ) ) []]

    , -- instanceElementShow
    instanceShow elDataName n

    , -- instanceElementInitialState

    let elType = newName "elType"
        state = newName "state"
        reqAttributes = list [ attrHaskellName name 
                             | (AttDef name attType REQUIRED) <- attdefList ]
    in instanceD (cxt []) (appTn (conT ''InitialState) [conT elType, conT state]) 
            [funD 'initialState [ clause [wildP] (normalB (
              sigE (varE 'undefined) 
                   (appTn (conT 'Element) [conT elDataName, reqAttributes, list [], elementState content] )
              ) ) []] ]
    ]

class InitialState elType state where
  initialState :: elType -> state



dtdToTypes :: FilePath -> Q [Dec]
dtdToTypes file = do
  read <- runIO $ liftM (dtdParse' file) $ readFile file
  case read of
    Left a -> fail a
    Right (Just (DTD name mbExtId decls))  -> do
    let zipped = zipElements decls
    let attrNamesUniq = nub $ map (\(AttDef n _ _ _) -> n) 
                            $ concatMap ( (\(AttListDecl _ l) -> l) . snd . snd ) zipped
    -- shared attribute names and instances 
        attrDecs = liftM concat $ map (\ n -> 
                      do let name = (attrHaskellName n)
                         d <- dataD (cxt []) name [] [] []
                         s <- instanceShow name
                         i <- instanceOfSimple (conT ''AttributeType) (conT $ mkName name)
                         return [d,s,i] ) attrNamesUniq

    -- | elements and attribute data belonging to it
    types <- liftM concat $ mapM toCode $ zipped

    let all = attrDecs ++ types
    runIO $ do -- for debugging purposes print generated code
       putStrLn "============= generated code ========================================="
       mapM_ (putStrLn . pprint) all
       putStrLn "============= generated code end ====================================="
       hFlush stdout
    return all
