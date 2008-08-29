{-# LANGUAGE StandaloneDeriving,  MultiParamTypeClasses, TemplateHaskell #-}
module Text.XML.Validated.TH (
  dtdToTypes
  -- exported for XmlToQ.hs 
  , attrHaskell
  , elementHaskellName
) where
import Debug.Trace
import Data.HList
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Control.Monad
import Text.XML.HaXml.Types as H
import Text.XML.HaXml.Parse
import qualified Data.Map as M
import Text.XML.Validated.Util
import Text.XML.Validated.Types
import Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import System.IO
import System.Directory
import System.FilePath
import Text.XML.Validated.Types as T
import Control.Monad.State as ST

showGeneratedCode = False
writeGeneratedCodeToFile = True
mytrace _ a = a

-- if there is an attribute called foo and an elemente called foo
-- teh same type can be used because they are used in different contexts
-- However we can't declare them twice, that's why the list defined is kept
data DataState = DataState {
  mPred :: Maybe String -- which type has been assigned a type number before this?
  , defined :: S.Set String
  }
initialDataState = DataState Nothing S.empty

-- all the template stuff


zipElements :: [MarkupDecl] -> [ ( String, (Maybe ElementDecl, Maybe AttListDecl)) ]
zipElements list =
        let filtered@((n, _):_) = catMaybes . map toMbTuple $ list
            in rootFst n . map addD . M.toList . M.fromListWithKey combine $ filtered
  where toMbTuple (Element eDecl@(ElementDecl n _)) = Just (n , (Just eDecl, Nothing) )
        toMbTuple (AttList aDecl@(AttListDecl n _)) = Just (n , (Nothing, Just aDecl) )
        toMbTuple _ = trace ("!! warning, still Non-exhaustive patterns in toMbTuple in TH.hs") Nothing
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



#ifdef TypeToNatTypeEq
instanceOfTypeToNat :: Maybe String -> TH.Name -> DecQ
instanceOfTypeToNat Nothing name =
        instanceD (cxt [])
                    (appTn (conT ''T.TypeToNat) [conT name, conT ''HZero])  []
instanceOfTypeToNat (Just pred) name =
        let nr = mkName "nr"
        in instanceD (cxt [
                  appTn (conT ''TypeToNat) [conT (mkName pred), (varT nr)]
                ])  -- (CreateEl <type> el)
                (appTn (conT ''T.TypeToNat) [conT name, appT (conT ''HSucc) (varT nr)])  []
#endif

attrHaskell = (++ "_A") . fstUpper
attrHaskellName = mkName . attrHaskell
elementHaskell = (++ "_T") . fstUpper
elementHaskellName = mkName .  elementHaskell

-- initial state ready for starting type based parser
conEl = (conT . elementHaskellName) -- private
elementState :: String -> ContentSpec -> TypeQ
elementState el H.EMPTY = conT ''T.EMPTY
elementState el ANY = conT ''T.ANY
elementState el (Mixed PCDATA) = addMod Star $ conT ''T.PCDATA
elementState el (Mixed (PCDATAplus list)) = addMod Star $ choiceList $
                                              (conT ''T.PCDATA)
                                              : [ appT (conT ''T.Elem) (conEl n)
                                                     | n <- list ]
elementState el (ContentSpec spec) = sp spec

sp (TagName n mod)     = addMod mod $ appT (conT ''T.Elem) (conEl n)
sp (Choice [item] mod) = addMod mod $ sp item
sp (Choice list mod)   = addMod mod $ choiceList $ map sp list
sp (Seq [item] mod)    = addMod mod $ sp item
sp (Seq list mod)      = addMod mod $ seqList $ map sp list
addMod :: Modifier -> TypeQ -> TypeQ
addMod None x = x
addMod Query x = appT (conT ''Query) x
addMod Star x = appT (conT ''Star) x
addMod Plus x =  seqList [x, appT (conT ''Star) x ]
-- (x)+ is rewritten seq [x, x*]
-- only for debugging:
deriving instance Show Mixed
deriving instance Show ContentSpec
deriving instance Show TokenizedType
deriving instance Show EnumeratedType
deriving instance Show FIXED
deriving instance Show DefaultDecl
deriving instance Show AttType
deriving instance Show AttDef
deriving instance Show AttListDecl
deriving instance Show ElementDecl
deriving instance Show MarkupDecl
deriving instance Show EntityDecl
deriving instance Show NotationDecl
deriving instance Show Misc
deriving instance Show PublicID
deriving instance Show GEDecl
deriving instance Show PEDecl
deriving instance Show EntityDef
deriving instance Show NDataDecl

toCode ::  ( String, (Maybe ElementDecl, Maybe AttListDecl)) -> StateT DataState Q [Dec]
toCode (n, (Just (ElementDecl _ content), Just (AttListDecl _ attdefList) ) ) = do
  let elDataName  =  elementHaskellName n

  mbPred <- gets mPred
  modify (\s -> s { mPred = Just (elementHaskell n) } )

  ST.lift $ sequence [

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

     let attr a = appT (conT ''A) a

         reqAttributes =    hlist [ attr $ conT $ attrHaskellName name
                                 | (AttDef name attType REQUIRED) <- attdefList ]
         allowedAttributes = hlist [ attr $ conT $ attrHaskellName name
                                 | (AttDef name attType _) <- attdefList ]
         state = mytrace (show attdefList) $ appT (conT ''T.NYV) $
                                                appTn (conT ''T.Element)
                                                      [conT elDataName
                                                      , appTn (conT ''AS)
                                                         [reqAttributes, allowedAttributes
                                                         , hlist [] {- added -}]
                                                      , elementState n content
                                                      , conT ''HFalse
                                                      ]
    in instanceD (cxt [])  -- (CreateEl <type> el)
                (appTn (conT ''T.InitialState) [conT elDataName, state])  []
           -- [funD 'T.initialState [ clause [wildP] (normalB ( varE 'undefined )) []] ]

#ifdef TypeToNatTypeEq
    , -- type to nat
    instanceOfTypeToNat mbPred elDataName
#endif
    ]

class InitialState elType state where
  initialState :: elType -> state



dtdToTypes :: FilePath -> XmlIds -> Q [Dec]
dtdToTypes file (XmlIds pub sys) = (flip evalStateT) (initialDataState) $ do
  read <- ST.lift $ runIO $ liftM (dtdParse' file) $ readFile file
  case read of
    Left a -> fail a
    Right (Just (DTD name mbExtId decls))  -> do
    let zipped = zipElements decls
    let attrNamesUniq = nub $ map (\(AttDef n _ _) -> n)
                            $ concatMap ( (\(Just (AttListDecl _ l)) -> l) . snd . snd ) zipped
    -- shared attribute names and instances
    attrDecs <- liftM concat $ mapM (\ n ->
                  do let name = attrHaskell n
                         nameN = mkName name
                     mbPred <- gets mPred
                     modify (\s -> s { mPred = Just name } )
                     ST.lift $ do
                       d <- dataD (cxt []) nameN [] [] []
                       s <- instanceShow nameN n
                       i <- instanceOfSimple (conT ''AttributeType) (conT nameN)
                       add <- let [el,val] = map mkName ["el", "val"]
                              in  funD (mkName $ fstLower name)
                                    [ clause [ varP el, varP val ]
                                        (normalB $ appEn (varE 'addAttrT)
                                                          [ varE el
                                                          , sigE (varE 'undefined) (conT $ nameN)
                                                          , varE val ]
                                        ) [] ]
#ifdef TypeToNatTypeEq
                       n <- instanceOfTypeToNat mbPred nameN
#endif
                       return [d,s,i
#ifdef TypeToNatTypeEq
                              ,n
#endif
                              ,add] ) attrNamesUniq

    -- | elements and attribute data belonging to it
    types <- liftM concat $ mapM toCode zipped

    docClass <- ST.lift $
      let toMaybeStr Nothing = conE 'Nothing
          toMaybeStr (Just s) = appE (conE 'Just) (litE . stringL $ s)
      in sequence [
        instanceD (cxt []) (appT (conT ''XmlIdsFromRoot) ( (conT . elementHaskellName . fst . head) zipped) )
            [ funD 'xmlIds [ clause [wildP] (normalB (appEn (conE 'XmlIds) (map toMaybeStr [pub, sys]))) []] ]
        ]

    let all = attrDecs ++ types ++ docClass
    let text = unlines $ ("-- ============= generated code based on " ++ file)
                         : map pprint all
                         ++ [ "-- ============ generated code end =====================================" ]
    when showGeneratedCode $ ST.lift $ runIO $ do -- for debugging purposes print generated code
             hPutStrLn stdout text
             hFlush stdout
    when writeGeneratedCodeToFile $ ST.lift $ runIO $ do
       let dir = "autogenerated-code"
       createDirectoryIfMissing False dir
       writeFile (dir </> (takeFileName file)) text

    return all
