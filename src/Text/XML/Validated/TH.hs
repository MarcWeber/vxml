{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell #-}
module Text.XML.Validated.TH where
import Data.HList
import Data.List
import Data.Maybe
import Control.Monad
import Text.XML.HaXml.Types as H
import Text.XML.HaXml.Parse
import qualified Data.Map as M
import Text.XML.HaXml.ParseLazy
import Text.XML.Validated.Util
import Text.XML.Validated.Types
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import System.IO
import Text.XML.Validated.Types as T

-- all the template stuff 
 

zipElements :: [MarkupDecl] -> [ ( String, (Maybe ElementDecl, Maybe AttListDecl)) ]
zipElements list = 
        let filtered@((n, _):_) = catMaybes . map toMbTuple $ list
            in rootFst n . map addD . M.toList . M.fromListWithKey combine $ filtered
  where toMbTuple (Element eDecl@(ElementDecl n _)) = Just (n , (Just eDecl, Nothing) )
        toMbTuple (AttList aDecl@(AttListDecl n _)) = Just (n , (Nothing, Just aDecl) )
        combine _ (Nothing, Just a) (Just e, Nothing) = (Just e, Just a)
        combine _ (Just e, Nothing) (Nothing, Just a) = (Just e, Just a)
        combine k _ _ = error $ "something went wrong: one element or attr named " 
                    ++ k ++ " list has been declared twice"
        addD (n, (Nothing, _)) = error $ "attr declarations, but no element name d" ++ n
        addD (n, (a, Nothing)) = (n, (a, Just (AttListDecl n []))) -- add missing att declarations 
        addD a = a
        -- make the fst element be root / head of list again 
        rootFst n l = filter ((== n) . fst) l ++ filter ((/= n) . fst) l

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
conEl = (conT . elementHaskellName) -- private 
elementState :: String -> ContentSpec -> TypeQ
elementState el H.EMPTY = conT ''T.EMPTY
elementState el ANY = error "ANY not yet supported"
elementState el (Mixed _) = error "MIXED not yet supported"
elementState el (ContentSpec spec) = sp spec
  where sp (TagName n mod)     = addMod mod $ appT (conT ''T.Elem) (conEl n)
        sp (Choice [item] mod) = addMod mod $ sp item
        sp (Choice list mod)   = addMod mod $ choiceList $ map sp list
        sp (Seq [item] mod)    = addMod mod $ sp item
        sp (Seq list mod)      = addMod mod $ seqList $ map sp list
        addMod :: Modifier -> TypeQ -> TypeQ
        addMod None x = x
        addMod Query x = appT (conT ''Query) x
        addMod Star x = appT (conT 'Star) x
        addMod Plus x =  seqList [x, appT (conT ''Star) x ]
                                              -- (x)+ is rewritten seq [x, x*]

toCode ::  ( String, (Maybe ElementDecl, Maybe AttListDecl)) -> Maybe String -> Q [Dec]
toCode (n, (Just (ElementDecl _ content), Just (AttListDecl _ attdefList) ) ) mbPred = do
  let elDataName  =  elementHaskellName n

  sequence [


      -- | dataElement
    dataD (cxt []) elDataName [] [] []

    , -- instanceElementType
    instanceOfSimple (conT ''ElType) (conT elDataName)

    , -- function elname for convinience 
    let [elType, initialState, el] = map mkName [ "elType", "initialState", "el"]
    in sigD (mkName $ fstLower n) $ -- why do I need this type signature ? 
            forallT [elType, initialState, el]
                    (cxt [ appTn (conT ''T.InitialState) [ conT elDataName, varT initialState ] 
                         , appTn (conT ''CreateEl) [ conT elDataName, varT el ]
                         ]) (appTn (conT ''PT) [varT initialState, varT el]) 
    , funD (mkName $ fstLower n) 
         [ clause [] (normalB (appEn (varE 'createElT) [undType (conT elDataName)] ) ) []]

    , -- instanceElementShow
    instanceShow elDataName n

    , -- instanceElementInitialState

    let reqAttributes = hlist [ attrHaskellName name
                            | (AttDef name attType REQUIRED) <- attdefList ]
        state = appT (conT ''T.NYV) $ 
                  appTn (conT ''T.Element) [conT elDataName
                                           , appTn (tupleT 2) [reqAttributes, hlist []] 
                                           , elementState n content
                                           , conT ''HFalse ]
    in instanceD (cxt [])  -- (CreateEl <type> el)
                (appTn (conT ''T.InitialState) [conT elDataName, state])  []
           -- [funD 'T.initialState [ clause [wildP] (normalB ( varE 'undefined )) []] ]

    , -- type to nat 
    case mbPred of
      Nothing ->
        instanceD (cxt [])
                    (appTn (conT ''T.TypeToNat) [conT elDataName, conT ''HZero])  []
      Just name ->
        let nr = mkName "nr"
        in instanceD (cxt [
                  appTn (conT ''TypeToNat) [conT (elementHaskellName name), (varT nr)]
                ])  -- (CreateEl <type> el)
                (appTn (conT ''T.TypeToNat) [conT elDataName, appT (conT ''HSucc) (varT nr)])  []
    ]

class InitialState elType state where
  initialState :: elType -> state



dtdToTypes :: FilePath -> XmlIds -> Q [Dec]
dtdToTypes file (XmlIds pub sys) = do
  read <- runIO $ liftM (dtdParse' file) $ readFile file
  case read of
    Left a -> fail a
    Right (Just (DTD name mbExtId decls))  -> do
    let zipped = zipElements decls
    let attrNamesUniq = nub $ map (\(AttDef n _ _) -> n) 
                            $ concatMap ( (\(Just (AttListDecl _ l)) -> l) . snd . snd ) zipped
    -- shared attribute names and instances 
    attrDecs <- liftM concat $ mapM (\ n -> 
                  do let name = (attrHaskellName n)
                     d <- dataD (cxt []) name [] [] []
                     s <- instanceShow name n
                     i <- instanceOfSimple (conT ''AttributeType) (conT name)
                     return [d,s,i] ) attrNamesUniq

    -- | elements and attribute data belonging to it
    types <- liftM concat $ mapM (uncurry toCode) $ zip zipped (Nothing : map (Just . fst) zipped)

    docClass <- 
      let toMaybeStr Nothing = varE 'Nothing
          toMaybeStr (Just s) = appE (conE 'Just) (litE . stringL $ s)
      in sequence [
        instanceD (cxt []) (appT (conT ''XmlIdsFromRoot) ( (conT . elementHaskellName . fst . head) zipped) )
            [ funD 'xmlIds [ clause [wildP] (normalB (appEn (conE 'XmlIds) (map toMaybeStr [pub, sys]))) []] ]
        ]

    let all = attrDecs ++ types ++ docClass
    runIO $ do -- for debugging purposes print generated code
       putStrLn "============= generated code ========================================="
       mapM_ (putStrLn . pprint) all
       putStrLn "============= generated code end ====================================="
       hFlush stdout
    return all
