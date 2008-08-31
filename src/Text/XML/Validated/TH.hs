{-# LANGUAGE PatternSignatures,  StandaloneDeriving,  MultiParamTypeClasses, TemplateHaskell #-}
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
import qualified Text.XML.HaXml.Types as H
import Text.XML.HaXml.Parse
import qualified Data.Map as M
import Text.XML.Validated.Util
import Text.XML.Validated.Types as VT
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
debugStateCount = True
debugStates = True

-- ========== state transformations for instances ====================
data ChildEl = PCDATA | T String 
  deriving ( Eq, Ord)
instance Show ChildEl where
  show PCDATA = "PCDATA"
  show (T s) = s
type ESpecOrTrans = Either ChildSpec (Int, NextState)
data ChildSpec = Child ChildEl
               | Seq [ ESpecOrTrans ]
               | Choice (Maybe Int) -- use this state transformation id (used for loopbacks in () * )
                        [ ESpecOrTrans ] -- Right is only used from Star
               | Star ChildSpec
               | Query ChildSpec
               | Plus ChildSpec
       deriving (Show)

data StateList =StateList {
                  -- next state id
                  nextId :: Int
                  -- state reduction id
                , stateReduction ::  M.Map NextState (Int, NextState)
                }
instance Show StateList where
  show (StateList nId sr) = unlines $ 
      ("nextId " ++ (show nId)) 
      : (map toStr $ M.elems sr)
    where toStr (id, ns) = unlines [ "id :" ++ show id, show ns ]

data NextState = NextState {
                   endable ::  Bool -- Bool = true means element can end here
                 , msg :: String -- error message on no match
                 , elmap ::  M.Map ChildEl -- given this child to add reduce to the following state
                                 TrState -- this state id (Nothing = consumed)
                 }
            deriving ( Eq, Ord)
instance Show NextState where
  show (NextState endable msg elmap) = unlines $ map ("  " ++ )
    [ "endable : " ++ (show endable)
    , "msg : " ++ msg 
    ] ++ [  "  " ++ (show child) ++ " -> " ++ show trS
         | (child, trS) <- M.toList elmap ]
data TrState =  C
            | St Int   -- Int
            deriving (Show, Eq, Ord)
emptyStateList = StateList 1 M.empty

-- if a next state reduction rule is found return its id else add it and return
-- new id
findOrAdd :: NextState -> ST.State StateList Int
findOrAdd ns = do
  (StateList nId sr) <- get
  case M.lookup ns sr of
    Nothing -> do
      put $ StateList (nId + 1) $
              M.insert ns (nId, ns) sr
      return nId
    Just i -> (return . fst) i
findOrAdd' ns = do
    i <- findOrAdd $ ns
    return (i, ns)

contentToState :: Bool  -- may the element be closed without adding further elements?
    -> Maybe (Int, NextState) -- continue with this state transformation
    -> ChildSpec
    -> ST.State StateList
            (Int, NextState) -- a NexState may be melted with others in
            -- Choice, so return it and make the caller responsible for adding it
            -- to StateList, Int is the state id, it must be known for self references in ()*
            -- (so (a| (b, c) *) will be realised as
            --          ^1 ^2
            -- a| b -> entry point to (b,c)*, starting at 2 continuing at 1 or at the passed continuation
contentToState e mbCont (Child c) = do
    let cont = maybe C (\(i, _) -> St i) mbCont
    findOrAdd' $ NextState e (show c) (M.singleton c cont)
contentToState _ _ (Seq []) = error "empty seq"
contentToState e Nothing (Seq [Left c]) = contentToState e Nothing c
contentToState e Nothing (Seq [Right c]) = return c
contentToState e mbCont (Seq ((Right _):_)) = error "Right should have been the last element passed by (3)"
contentToState e mbCont (Seq ((Left x):xs)) = do
    cont <- contentToState False mbCont $ Seq xs
    contentToState e (Just cont) x
contentToState e mbCont (Choice mbUseId list) = do
    list' <- mapM (toTransform e mbCont) list
    let maps = map (elmap . snd) list'
    let e' = any id $ map (endable . snd) list'
    when (not . M.null . foldr M.intersection M.empty $ maps) $ do
      error "non deterministic choice ? check your dtd with xmllint"
    let union = foldr M.union M.empty $ maps
    let msg' = case map (msg . snd) list' of
                  [x] -> x
                  xs -> "one of " ++ intercalate "," xs
    let ns = NextState (e' || e) msg' union
    maybe (findOrAdd' ns)
          (\id -> modify (\s -> s { stateReduction  = M.insert ns (id, ns) (stateReduction s) }) -- (1) Just is only passed from here
                  >> return (id, ns)
          )
          mbUseId
  where toTransform _ mbCont (Right x) = return x
        toTransform e mbCont (Left c) = contentToState e mbCont c
contentToState e (Just cont) (Query chd ) =
    contentToState e Nothing $ Choice Nothing [ Right cont, Left (Seq [ Left chd, Right cont {- (3) -}]) ]
contentToState e Nothing (Query chd ) =
    contentToState True Nothing chd
contentToState e mbCont (Star chd) = do
  -- TODO look for existing entries
  (StateList nId sr) <- get -- new id to be passed as continuation
  put $ StateList (nId + 1) sr
  let zeroTimesMatch =
        case mbCont of
          Nothing -> []
          Just c -> [ Right c ]

  chd' <- contentToState e (Just (nId, NextState (error "X") "one or more of something" (error "X") ) {- loop back -} ) chd
  contentToState (e || isNothing mbCont) Nothing $ Choice (Just nId) {- (1) -} $
      (Right chd' {- one or more -}) : zeroTimesMatch
contentToState e mbCont (Plus a) = contentToState e mbCont $ Seq [ Left a, Left (Star a) ]

stateName' ::  Int -> String
stateName' i = "State" ++ (show i)
stateName :: Int -> Name
stateName = mkName . stateName'

-- ========== TH code generating instances etc =======================

-- if there is an attribute called foo and an elemente called foo
-- then same type could be used because they are used in different contexts
-- However we can't declare them twice, that's why the list defined is kept
data DataState = DataState {
#ifdef TypeToNatTypeEq
  mPred :: Maybe String, -- which type has been assigned a type number before this?
#endif
    defined :: S.Set String -- list of already defined "data Att or El" names
  , stateList :: StateList  -- try reusing state transformations to not get too many instances..
  , realised :: S.Set Int -- list of already realised state state transformations
                    -- (thus class and data type already has been generated)
  }
initialDataState = DataState 
#ifdef TypeToNatTypeEq
        Nothing 
#endif
        S.empty emptyStateList S.empty

-- all the template stuff


zipElements :: [H.MarkupDecl] -> [ ( String, (Maybe H.ElementDecl, Maybe H.AttListDecl)) ]
zipElements list =
        let filtered@((n, _):_) = catMaybes . map toMbTuple $ list
            in rootFst n . map addD . M.toList . M.fromListWithKey combine $ filtered
  where toMbTuple (H.Element eDecl@(H.ElementDecl n _)) = Just (n , (Just eDecl, Nothing) )
        toMbTuple (H.AttList aDecl@(H.AttListDecl n _)) = Just (n , (Nothing, Just aDecl) )
        toMbTuple _ = trace ("!! warning, still Non-exhaustive patterns in toMbTuple in TH.hs") Nothing
        combine _ (Nothing, Just a) (Just e, Nothing) = (Just e, Just a)
        combine _ (Just e, Nothing) (Nothing, Just a) = (Just e, Just a)
        combine k _ _ = error $ "something went wrong: one element or attr named "
                    ++ k ++ " list has been declared twice"
        addD (n, (Nothing, _)) = error $ "attr declarations, but no element name d" ++ n
        addD (n, (a, Nothing)) = (n, (a, Just (H.AttListDecl n []))) -- add missing att declarations
        addD a = a
        -- make the fst element be root / head of list again
        rootFst n l = filter ((== n) . fst) l ++ filter ((/= n) . fst) l

instanceShow dataName value = do
  instanceD (cxt []) (appTn (conT ''Show) [conT dataName])
    [funD 'show [ clause [wildP] (normalB (litE . stringL $ value)) []] ]


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

#ifdef DoValidate
-- initial state ready for starting type based parser
conEl = (conT . elementHaskellName) -- private
elementState :: String -> H.ContentSpec -> StateT DataState Q (TypeQ, [DecQ])
elementState n H.EMPTY = return $ (conT ''T.EMPTY, [])
elementState n H.ANY = return $ (conT ''T.ANY, [])
elementState n (H.Mixed H.PCDATA) = return $ (conT ''T.PCDATA, [])
elementState n (H.Mixed (H.PCDATAplus list)) = 
    realise n $ contentToState False Nothing $ Star $ Choice Nothing $
        Left (Child PCDATA) : map (Left . Child . T) list
elementState n (H.ContentSpec spec) =
    realise n $ contentToStateE $ sp spec
sp :: H.CP -> ChildSpec
sp (H.TagName n mod)     = addMod mod $ Child $ T n
sp (H.Choice [item] mod) = addMod mod $ sp item
sp (H.Choice list mod)   = addMod mod $ Choice Nothing $ map (Left . sp) list
sp (H.Seq [item] mod)    = addMod mod $ sp item
sp (H.Seq list mod)      = addMod mod $ Seq $ map (Left . sp) list
addMod :: H.Modifier -> ChildSpec -> ChildSpec
addMod H.None x = x
addMod H.Query x = Query x
addMod H.Star x = Star x
addMod H.Plus x =  Plus x
contentToStateE  (Star a) = contentToState True Nothing (Star a)
contentToStateE  (Query a) = contentToState True Nothing (Query a)
contentToStateE  a = contentToState False Nothing a

-- given a list of state transformations, the state which already have been realised and an id
-- of the start state which should be realized return a list of declarations
realise :: String -> ST.State StateList (Int, NextState) -> ST.StateT DataState Q (TypeQ , [ DecQ ] )
realise n stF = do
    (gs :: DataState) <- ST.get
    let ((id,_), stList') = ST.runState stF (stateList gs :: StateList)
        stListRev = (M.fromList . M.elems . stateReduction ) stList'  -- to Map id NextState
        sts = S.fromList $ usedSTS (S.empty) stListRev id
        new = sts `S.difference` (realised gs)
    put $ gs { stateList = stList', realised = (realised gs) `S.union` new }
    when debugStateCount $ ST.lift $ runIO $ 
      hPutStrLn stdout $ n ++ " states: " ++ (show $ S.size sts) ++ " new : " ++ (show $ S.size new)
    return $ ( conT $ stateName id
             , concatMap (\id -> realiseST id . fromJust . M.lookup id $ stListRev) (S.elems new))
  where 
        realiseST id (NextState endable msg elmap) =
          let n = stateName id
              instances = let el PCDATA = conT ''T.PCDATA
                              el (T s) = appT (conT ''T.Elem) (conT $ elementHaskellName s)
                              st C = conT ''T.C
                              st (St id) = conT $ stateName id
                          in map ( \(chd, trState) -> 
                                      instanceD (cxt []) (appTn (conT ''T.Consume) [conT n, el chd , st trState]) []
                                 ) $ M.toList elmap
          in [ dataD (cxt []) n [] [] [] ]
             ++ instances
             -- TODO remove this instance and Retry alltogether if the state machine works well for bis dtds such as XHTML 
             ++ [ let [a,el]  = map mkName ["a", "el"]
                  in instanceD (cxt []) ( appTn (conT ''T.Retry) [ varT el, conT n, varT a, conT n ]) [] ]
             ++ ( if endable 
                  then let elType = mkName "elType"
                       in [ instanceD (cxt []) (appTn (conT ''T.ElEndable) [ varT elType, conT n])  [] ]
                  else [] )

        usedSTS :: S.Set Int -> M.Map Int NextState -> Int -> [Int]
        usedSTS visited map id = 
          let (NextState _ _ elmap) = (fromJust . M.lookup id) map
          in id : ( [ id  | St id <- M.elems elmap, not $ S.member id visited  ] >>= usedSTS (S.insert id visited) map )




-- (x)+ is rewritten seq [x, x*]
-- only for debugging:
#else
elementState _ H.EMPTY = return $ (conT ''T.EMPTY, []) -- at least force that empty elements are still empty
elementState _ _ = return $ (conT ''NoValidation, [])
#endif

deriving instance Show H.Mixed
deriving instance Show H.ContentSpec
deriving instance Show H.TokenizedType
deriving instance Show H.EnumeratedType
deriving instance Show H.FIXED
deriving instance Show H.DefaultDecl
deriving instance Show H.AttType
deriving instance Show H.AttDef
deriving instance Show H.AttListDecl
deriving instance Show H.ElementDecl
deriving instance Show H.MarkupDecl
deriving instance Show H.EntityDecl
deriving instance Show H.NotationDecl
deriving instance Show H.Misc
deriving instance Show H.PublicID
deriving instance Show H.GEDecl
deriving instance Show H.PEDecl
deriving instance Show H.EntityDef
deriving instance Show H.NDataDecl

toCode ::  ( String, (Maybe H.ElementDecl, Maybe H.AttListDecl)) -> StateT DataState Q [Dec]
toCode (n, (Just (H.ElementDecl _ content), Just (H.AttListDecl _ attdefList) ) ) = do
  let elDataName  =  elementHaskellName n

#ifdef TypeToNatTypeEq
  mbPred <- gets mPred
  modify (\s -> s { mPred = Just (elementHaskell n) } )
#endif
  (es, states) <- elementState n content
  ST.lift $ sequence  ([

   -- | dataElement
   dataD (cxt []) elDataName [] [] []

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

     let
#ifdef DoValidate
         attr a = appT (conT ''A) a

         reqAttributes =    hlist [ attr $ conT $ attrHaskellName name
                                 | (H.AttDef name attType H.REQUIRED) <- attdefList ]
#endif
         state = mytrace (show attdefList) $
                  appT (conT ''T.NYV) $
                    appTn (conT ''T.Element)
                          [conT elDataName
                          , appTn (conT ''AS)
                             [
#ifdef DoValidate
                              reqAttributes
#else
                              hlist []
#endif
                             ,
                             hlist [] {- added -}
                             ]
                          , es
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
    ++ ( -- AttrOk instances
          [ instanceD (cxt ([])) (appTn (conT ''AttrOk) [ conT elDataName, conT $ attrHaskellName name ] ) []
          | (H.AttDef name attType _) <- attdefList ]
       )
    -- state data typens and state transforming instances
    ++ states
    )

dtdToTypes :: FilePath -> XmlIds -> Q [Dec]
dtdToTypes file (XmlIds pub sys) = (flip evalStateT) (initialDataState) $ do
  read <- ST.lift $ runIO $ liftM (dtdParse' file) $ readFile file
  case read of
    Left a -> fail a
    Right (Just (H.DTD name mbExtId decls))  -> do
    let zipped = zipElements decls
    let attrNamesUniq = nub $ map (\(H.AttDef n _ _) -> n)
                            $ concatMap ( (\(Just (H.AttListDecl _ l)) -> l) . snd . snd ) zipped
    -- shared attribute names and instances
    attrDecs <- liftM concat $ mapM (\ n ->
                  do let name = attrHaskell n
                         nameN = mkName name
#ifdef TypeToNatTypeEq
                     mbPred <- gets mPred
                     modify (\s -> s { mPred = Just name } )
#endif
                     ST.lift $ do
                       d <- dataD (cxt []) nameN [] [] []
                       s <- instanceShow nameN n
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
                       return [d,s
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
    gs <- ST.get
    let text = unlines $ ("-- ============= generated code based on " ++ file)
                         : map pprint all
                         ++ [ "-- ============ generated code end =====================================" ]
                         -- error messages 
                         ++ (  let map = (M.fromList . M.elems . stateReduction . stateList ) gs
                               in [ "err msg " ++ (stateName' i) ++ ": " 
                                    ++ (msg . fromJust $ M.lookup i map)
                                  | i <- S.elems (realised gs) ]
                            )
                         ++ if debugStates then [ "debug states: ", show (stateList gs)] else []

    when showGeneratedCode $ ST.lift $ runIO $ do -- for debugging purposes print generated code
             hPutStrLn stdout text
             hFlush stdout
    when writeGeneratedCodeToFile $ ST.lift $ runIO $ do
       let dir = "autogenerated-code"
       createDirectoryIfMissing False dir
       writeFile (dir </> (takeFileName file)) text

    return all
