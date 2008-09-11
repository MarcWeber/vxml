{-# LANGUAGE ScopedTypeVariables,  PatternGuards, StandaloneDeriving,  MultiParamTypeClasses, TemplateHaskell #-}
#if (__GLASGOW_HASKELL__ > 608)
  {-# LANGUAGE ScopedTypeVariables #-}
#else
  {-# LANGUAGE PatternSignatures #-}
#endif
module Text.XML.Validated.TH (
  dtdToTypes
  -- exported for XmlToQ.hs
  , simpleNameGenerator
  , intelligentNameGenerator
  , NameGenerator(..)
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

writeInfoFile = True -- reommended. You can lookup some cases why your docuemnts don't validate
showGeneratedCode = True -- if you are curious enable this to see the code beeing generated by template haskell
debugStateCount = True
debugStates = True -- enable this to have a look at the state reduction instances
mytrace _ a = a

fromJust' _ (Just x) = x
fromJust' s Nothing = error s

-- ========== state transformations for instances ====================
data ChildEl = PCDATA | T String
  deriving ( Eq, Ord)
instance Show ChildEl where
  show PCDATA = "PCDATA"
  show (T s) = s
type ESpecOrTrans = Either ChildSpec (Int, NextState)
data ChildSpec = Child ChildEl
               | Seq [ ESpecOrTrans ]
               | Choice [ ESpecOrTrans ] -- Right is only used from Star
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
                 , elmap ::  M.Map ChildEl -- given this child to add reduce to the following state
                                 TrState -- this state id (Nothing = consumed)
                 }
            deriving ( Eq, Ord)
endable' a b = trace a (endable b)
elmap' a b = trace a (elmap b)
instance Show NextState where
  show (NextState endable elmap) = unlines $ map ("  " ++ )
    [ "endable : " ++ (show endable)
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

    findOrAdd' $ NextState e (M.singleton c cont)
contentToState _ _ (Seq []) = error "empty seq"
contentToState e Nothing (Seq [Left c]) = contentToState e Nothing c
contentToState e Nothing (Seq [Right c]) = return c
contentToState e (Just cont) (Seq [Left c]) = contentToState e (Just cont) c
contentToState e (Just _) (Seq [Right _]) = error "doo!"
contentToState e mbCont (Seq ((Right _):_)) = error "Right should have been the last element passed by (3)"
contentToState e mbCont (Seq ((Left x):xs@(_:_))) = do
    cont <- contentToState False mbCont $ Seq xs
    contentToState e (Just cont) x
contentToState e mbCont (Choice list) =  error "aehm"
contentToState e (Just cont) (Query chd ) = error "aehm2"
contentToState e Nothing (Query chd ) =
    contentToState True Nothing chd
contentToState e mbCont (Star chd) = noDup $ do
  let contElmap = ( maybe (M.empty) (elmap . snd) mbCont)

  -- building NextState to be passed as continuation
  (StateList nId sr) <- get -- new id to be passed as continuation
  put $ StateList (nId + 1) sr
  let e' = trace ("nId " ++(show nId)) $  e || isNothing mbCont
  chdWithoutCont <- contentToState e' Nothing chd
    -- nsCont to be passed to real nsCont, breaking loop 
  c <- get
  let nsCont = trace ("without cond of " ++ (show nId) ++ (show c) ++ "end") $
                        (nId, NextState e'$ M.union
                                (M.map (const $ St nId) ((elmap . snd) chdWithoutCont)) -- loop back to this id 
                                contElmap -- or continue with mbCont
                       )
  chdWithCont <- contentToState False (Just nsCont) chd
  let ns = trace ("throwing awayy current :" ++(show nId) ++" , " ++ (show $ fst chdWithCont) ++ " list :" ++ (show c) ++ "<x<") $ NextState e' (M.union
              ((elmap . snd) chdWithCont)
              contElmap 
            )
  (StateList _ sr2) <- get
  case M.lookup ns sr2 of
    Just r@(iFound,n)  -> trace ("ALREADY PRESETN " ++ "replacing " ++   (show nId) ++ "," ++ (show ns) ++ " by " ++ (show iFound) ++ "," ++ (show n)) $ do 
                        -- oh? already present, then it can be reused. The id (loop) reference must be replaced by the found one
                        let mapTr C = C
                            mapTr (St i'') = if i'' == nId then St iFound else St i''
                            f ((NextState e elmap) ,(i,_)) = let ns' = NextState e (M.map mapTr elmap)
                                                             in (ns',(i,ns'))
                        modify (\s -> s { stateReduction = M.fromList . map f . M.toList $ stateReduction s } )
                        return r 
    Nothing -> do
      modify $ trace ("adding " ++(show nId)) $ \s -> s { stateReduction  = insert' (show nId) ns (nId, ns) (trace ("inserting into " ++(show s)) (stateReduction s)) } -- (1) Just is only passed from here
      t <- get
      return $ trace ("dana " ++(show nId) ++"  >> \n" ++(show t) ++ "\n  <<<<<<<<end") (nId, ns)
contentToState e mbCont (Plus a) = contentToState e mbCont $ Seq [ Left a, Left (Star a) ]

stateName' ::  Int -> String
stateName' i = "State" ++ (show i)
stateName :: Int -> Name
stateName = mkName . stateName'

insert' s k v map = if k `M.member` map then error $ "xx " ++ s
                  else M.insert k v map
-- Have a look at the returned id, if an equivalent state transformation path is already present return that instead

noDup :: ST.State StateList (Int, NextState) -> ST.State StateList (Int, NextState)
noDup f = do
    backup <- get
    r@(id,_) <- f
    sl' <- get
    case findDuplicate sl' id ( (map fst . M.elems . stateReduction) backup) of
      Nothing -> return r
      Just d -> do
        put ( trace ("found " ++ (show d) ++ " for " ++ (show id))  backup )
        return $ d
  where findDuplicate (StateList _ map) id ids =
          let m = (M.fromList . M.elems) map -- to Map id NextState
          in  (listToMaybe . filter (isDup m S.empty M.empty id) $ ids)
               >>=  \i -> Just (i, (fromJust' "a") $ M.lookup i m) -- add NextState
        isDup :: M.Map Int NextState
            -> S.Set Int  -- visited ids of first path
            -> M.Map Int Int -- mapping from id of the first path to corresponding ids of the snd
            -> Int -> Int -> Bool
        isDup _ _ _ a b | a == b = True
        isDup sl vis map id1 id2
          | (Just (NextState e1 m1)) <- M.lookup id1 sl -- if noDup is called while building Star path the loopback id is not yet present
          , (Just (NextState e2 m2)) <- M.lookup id2 sl -- in this case there might be a Nothing, duplication will be removed later than
              = let compare (C, C) = True
                    compare (St id1, St id2) =
                      let seen = id1 `S.member` vis
                      in id1 == id2  -- obviously the same
                            -- visited? then we must have seen the other id as well
                         || seen && maybe False (id2 ==) (M.lookup id1 map)
                            -- follow ids
                         || not seen && isDup sl (S.insert id1 vis) (M.insert id1 id2 map) id1 id2
                    compare _ = False
                in e1 == e2
                    && (M.keys m1) == (M.keys m2)
                    && (all compare $ zip (M.elems m1) (M.elems m2))
        isDup _ _ _ _ _ = False


-- ========== TH code generating instances etc =======================

-- if there is an attribute called foo and an elemente called foo
-- then same type could be used because they are used in different contexts
-- However we can't declare them twice, that's why the list defined is kept
data DataState = DataState {
    defined :: S.Set String -- list of already defined "data Att or El" names
  , stateList :: StateList  -- try reusing state transformations to not get too many instances..
  , realised :: S.Set Int -- list of already realised state state transformations
                    -- (thus class and data type already has been generated)
  , nameGen :: NameGenerator
  }
initialDataState = DataState S.empty emptyStateList S.empty (simpleNameGenerator undefined undefined)

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


#ifdef DoValidate
-- initial state ready for starting type based parser
elementState :: String -> H.ContentSpec -> StateT DataState Q (TypeQ, [DecQ])
elementState n H.EMPTY = return $ (conT ''T.EMPTY, [])
elementState n H.ANY = return $ (conT ''T.ANY, [])
elementState n (H.Mixed H.PCDATA) = return $ (conT ''T.PCDATA, [])
elementState n (H.Mixed (H.PCDATAplus list)) =
    realise n $ contentToState False Nothing $ Star $ Choice $
        Left (Child PCDATA) : map (Left . Child . T) list
elementState n (H.ContentSpec spec) =
    realise n $ contentToStateE $ sp spec
sp :: H.CP -> ChildSpec
sp (H.TagName n mod)     = addMod mod $ Child $ T n
sp (H.Choice [item] mod) = addMod mod $ sp item
sp (H.Choice list mod)   = addMod mod $ Choice $ map (Left . sp) list
sp (H.Seq [] mod)    = error "empty seq list passed from HaXmL ?" -- should not happen
sp (H.Seq [item] mod)    = addMod mod $ sp item
sp (H.Seq list@(_:_) mod)= addMod mod $ Seq $ map (Left . sp) list
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
        ng = nameGen gs
    put $ gs { stateList = stList', realised = (realised gs) `S.union` new }
    when debugStateCount $ ST.lift $ runIO $ do
      hPutStrLn stdout $ n ++ " states: " ++ (show $ S.size sts) ++ " new : " ++ (show $ S.size new)
      hFlush stdout
    return $ ( conT $ stateName id
             , concatMap (\id -> realiseST ng id . (fromJust' "b") . M.lookup id $ stListRev) (S.elems new))
  where
        realiseST ng id (NextState endable elmap) =
          let n = stateName id
              instances = let el PCDATA = conT ''T.PCDATA
                              el (T s) = appT (conT ''T.Elem) (conT $ mkName $ (dataElName ng) s)
                              st C = conT ''T.C
                              st (St id) = conT $ stateName id
                          in map ( \(chd, trState) ->
                                      instanceD (cxt []) (appTn (conT ''T.Consume) [conT n, el chd , st trState]) []
                                 ) $ M.toList elmap
          in [ dataD (cxt []) n [] [] [] ]
             ++ instances
             ++ ( if endable
                  then let elType = mkName "elType"
                       in [ instanceD (cxt []) (appTn (conT ''T.ElEndable) [ varT elType, conT n])  [] ]
                  else [] )

        usedSTS :: S.Set Int -> M.Map Int NextState -> Int -> [Int]
        usedSTS visited map id =
          let (NextState _ elmap) = case M.lookup id map of
                                    Just x -> x
                                    Nothing -> error (show id ++ " +++ " ++ show map)
          -- let (NextState _ elmap) = ((fromJust' ("c" ++  (show id))) . M.lookup id) map
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
  ng <- gets nameGen

  let elDataName = mkName $ (dataElName ng) n

  -- initial state 
  (st, states) <- elementState n content
  -- initial state attrs 
  let reqAttributes = hlist
          [ attr $ conT $ mkName $ (dataAttrName ng) name
          | (H.AttDef name attType H.REQUIRED) <- attdefList ]

      stA = appTn (conT ''AS) [
#ifdef DoValidate
                              reqAttributes
#else
                              hlist []
#endif
                             ,
                             hlist [] {- added -}
                             ]
      attr a = appT (conT ''A) a
      elst = appT (conT ''NYV) $ appTn (conT ''Element) [ conT elDataName, stA, st, conT ''HFalse ]
      classElem' = mkName $ (classElem ng) n
      elemName' = mkName $ (elemName ng) n
      runElemI' = mkName $ runElemI ng n
      runElemIT' = mkName $ runElemTI ng n

  let sigUnd =sigE (varE 'undefined)
  ST.lift $ sequence  (
   [

   -- | dataElement
     dataD (cxt []) elDataName [] [] []

   , -- instanceElementShow
     instanceShow elDataName n

   , -- function elname for convinience
   let [elType, initialState, el] = map mkName [ "elType", "initialState", "el"]
   in sigD (mkName $ (uiElName ng) n) $ -- why do I need this type signature ?
     forallT [elType, initialState, el]
       (cxt [ appTn (conT ''CreateEl) [ conT elDataName, varT el ]
            ]) (appTn (conT ''PT) [ elst, varT el])
   , funD (mkName $ (uiElName ng) n)
       [ clause [] (normalB ( appEn (conE 'PT) [ undType elst, appE (varE 'createEl) (undType (conT elDataName)) ])) []] 

   , -- runElem 
    {- runElemI :: VXML st el st2 el2 st3 el3 a -> (a, el3)
       runElemI (VXML f) = 
        let (a,p) = f id
        in  p (PT (undefined :: ) (createEl (undefined :: Elem)))
     - -}
     let [f] = map mkName ["f"]
     in funD runElemI' [ clause [conP 'VXML [varP f]] (normalB 
           [|
            let (a,p) = $(varE f) id
            in   (a, p (T.PT $(sigUnd elst) (createEl $(sigUnd $ conT elDataName))))
          |]) []]
    , funD (mkName $ runElem ng n) [ clause [] (normalB 
          [| (\(a,b) -> (a, T.fromPT b)) . $(varE $ mkName $ runElemI ng n) |]) []]

    , -- runElemT 
     let [f] = map mkName ["f"]
     in funD (mkName $ runElemTI ng n) [ clause [conP 'VXMLT [varP f]] (normalB 
           [| do
                  (a,p) <- $(varE f) id
                  return (a, p (T.PT $(sigUnd elst) (createEl $(sigUnd $ conT elDataName))))
          |]) []]
    , funD (mkName $ runElemT ng n) [ clause [] (normalB 
          [| liftM (\(a,b) -> (a, T.fromPT b)) . $(varE $ mkName $ runElemTI ng n) |]) []]

     -- monadic like interface 
   , -- class 
   {-
    class A_C m elc stc3 elc3  st2 el2 st3 el3 
          | st2 stc3 -> st3, el2 elc3 -> el3 where
      aC :: m AA  elc  AA elc  stc3 elc3 a
          -> m st el  st2 el2  st3 el3 a
    -}
    let [ m, elc, stc3, elc3, el2, st2, el3, st3, a, el, st] = map mkName  ["m", "elc", "stc3", "elc3", "el2", "st2", "el3", "st3", "a", "el", "st"]
    in classD (cxt [])
              (classElem') 
              [ m, elc, stc3, elc3, st2, el2, st3, el3 ]
              [{-funDep [st2,stc3] [st3],-}FunDep [el2,elc3] [el3]] 
              [sigD elemName' 
                    (forallT [a, st, el]
                             (cxt [])
                             ( appTn arrowT 
                               [ appTn (varT m) [ elst, varT elc, elst, varT elc, varT stc3, varT elc3, varT a]
                               , appTn (varT m) (map varT [st, el, st2, el2, st3, el3, a]) ]))
                             ]
   , -- instance VXML 
   {-
   instance ( AddElT st2 el2  stc3 elc3 st3 el3
          , CreateEl A_T elc
          -- , EndAttrsEndElement Root_T el2 el
          ) => A_C VXML elc  stc3 elc3  st2 el2 st3 el3 where
    aC f = let (a, child) = runA' f
           in VXML $ \p -> (a, (`addElT` child) . p)
    -}


   let [ st2, el2, st3, el3, stc3, elc3,  elc, f ] = map mkName [ "st2", "el2", "st3", "el3", "stc3", "elc3", "elc", "f"] 
   in instanceD  (cxt [ appTn (conT ''T.AddElT) [ varT st2, varT el2, varT stc3, varT elc3, varT st3, varT el3 ]
                     , appTn (conT ''T.CreateEl) [ conT elDataName, varT elc]
                     ])
                 (appTn (conT classElem') (conT ''VXML : map varT [elc, stc3, elc3, st2, el2, st3, el3]))
                 [ funD elemName' [ clause [varP f] (normalB 
                 [| let (a, child) = $(varE runElemI') $(varE f)
                    in T.VXML $ \p -> (a, (\b -> T.addElT b child) . p)
                 |]) []] ]
   , -- instance VXMLT
   {-
     instance ( Monad m
            , AddElT st2 el2 stc3 elc3 st3 el3
            , CreateEl A_T elc
            , EndAttrsEndElement Root_T el2 el
            ) => A_C (VXMLT m) elc  stc3 elc3  st2 el2 st3 el3 where
      aC f = VXMLT $ \p -> do
             (a, child) <- runAT' f
             return (a, (`addElT` child) . p)
   -}
   let [ st2, el2, st3, el3, stc3, elc3,  elc, f, m ] = map mkName [ "st2", "el2", "st3", "el3", "stc3", "elc3", "elc", "f", "m"] 
   in instanceD  (cxt [ appT (conT ''Monad) (varT m)
                     , appTn (conT ''T.AddElT) [ varT st2, varT el2, varT stc3, varT elc3 , varT st3, varT el3]
                     , appTn (conT ''T.CreateEl) [ conT elDataName, varT elc]
                     ])
                 (appTn (conT classElem') (appT (conT ''VXMLT) (varT m) : map varT [elc, stc3, elc3, st2, el2, st3, el3]))
                 [ funD elemName' [ clause [varP f] (normalB 
                 [| T.VXMLT $ \p -> do
                             (a, child) <- $(varE runElemIT') $(varE f)
                             return (a, (\b -> T.addElT b child) . p)
                 |]) []] ]
   ] ++ ( -- AttrOk instances
          [ instanceD (cxt ([])) (appTn (conT ''AttrOk)
                                        [ conT elDataName
                                        , conT $ mkName $ (dataAttrName ng) name ] ) []
          | (H.AttDef name attType _) <- attdefList ]
       )
    -- state data typens and state transforming instances
    ++ states
    )

data NameGenerator = NameGenerator {

  -- internal representation of types
    dataElName :: String -> String
  , dataAttrName :: String -> String

  -- old interface  (still used by tests)
  , uiElName :: String -> String
  , uiAddAttr :: String -> String

  -- monadic like interface 
  , classElem :: String -> String
  , classAttr :: String -> String
  , elemName :: String -> String
  , elemAttr :: String -> String
  , runElemI :: String -> String -- internal version of runElem 
  , runElem :: String -> String --  (\(a,b) -> (a, fromPT b)) . runElemI
  , execElem :: String -> String
  , evalElem :: String -> String
  , runElemTI :: String -> String
  , runElemT :: String -> String
  , execElemT :: String -> String
  , evalElemT :: String -> String
  }

-- used by test cases, default is intelligentNameGenerator
simpleNameGenerator :: [ String ] -> [ String ] -> NameGenerator
simpleNameGenerator _ _ = NameGenerator {
    dataElName = (++ "_T") . fstUpper
  , dataAttrName = (++ "_A") . fstUpper

  -- old interface  (still used by tests)
  , uiElName = (++ "_TT")
  , uiAddAttr = (++ "_AA")

  -- monadic like interface  
  , classElem = ( ++ "_C") . fstUpper
  , classAttr = ( ++ "_CA") . fstUpper
  , elemName = fstLower  -- monadic element name of class classElem 
  , elemAttr = (++ "A") . fstLower -- append _A to never conflict with elem 
  , runElem = ("run" ++) . fstUpper
  , runElemI = (\s -> "run" ++ s ++ "'") . fstUpper
  , execElem = ("exec" ++) . fstUpper
  , evalElem = ("eval" ++) . fstUpper
  , runElemT = (\s -> "run" ++ s ++ "T" ) . fstUpper
  , runElemTI = (\s -> "run" ++ s ++ "T'" ) . fstUpper
  , execElemT = (\s -> "exec" ++ s ++ "T" ) . fstUpper
  , evalElemT = (\s -> "eval" ++ s ++ "T" ) . fstUpper
}

noKeyWords s = fromMaybe s $ M.lookup s map
  where map = M.fromList [("type", "typE")]

-- same as simpleNameGenerator but it only adds A to the attribute name if
-- there is an element having the same name
intelligentNameGenerator :: [ String ] -> [ String ] -> NameGenerator
intelligentNameGenerator els attrs =
  (simpleNameGenerator undefined undefined) {
      elemName = noKeyWords . fstLower
    , elemAttr = let an :: String -> String
                     an n = if n `elem` els then n ++ "A" else n
                     map' = M.fromList $ map (\n -> (n, an n)) attrs
                 in  (fromJust' "d") .(flip M.lookup) map'
  }

dtdToTypes :: Maybe ([String] {- el names -} -> [String] {- attr names -} -> NameGenerator)
              -> FilePath -> XmlIds -> Q [Dec]
dtdToTypes mbNG file (XmlIds pub sys) = do
  read <- runIO $ liftM (dtdParse' file) $ readFile file
  case read of
    Left a -> fail a
    Right (Just (H.DTD name mbExtId decls))  ->
      let
          zipped = zipElements decls
          attrNamesUniq = nub $ map (\(H.AttDef n _ _) -> n)
                              $ concatMap ( (\(Just (H.AttListDecl _ l)) -> l) . snd . snd ) zipped
          ng = (fromMaybe intelligentNameGenerator mbNG) (map fst zipped) attrNamesUniq

      in (flip evalStateT) (initialDataState { nameGen = ng } ) $ do
        -- shared attribute names and instances
        attrDecs <- 
          liftM concat $ mapM (\ n ->
            do let name = (dataAttrName ng) n
                   nameN = mkName name
                   cName = mkName $ (classAttr ng) n
                   uiName = (uiAddAttr ng) n
                   elemAttrName = mkName $ (elemAttr ng) n
               ST.lift $ do
                 d <- dataD (cxt []) nameN [] [] []
                 s <- instanceShow nameN n
                 add <- let [el,val] = map mkName ["el", "val"]
                        in  funD (mkName uiName)
                             [ clause [ varP el, varP val ]
                                (normalB $ appEn (varE 'addAttrT)
                                                  [ varE el
                                                  , sigE (varE 'undefined) (conT $ nameN)
                                                  , varE val ]
                                ) [] ]
                 aClass <- 

                          {- class A_CA m st el st2 el2  st3 el3 value 
                              | st -> st2, st2 -> st3
                              , st st2 el2 -> el
                              , st st3 el3 -> el
                              --, st2 st3 el3 -> el2
                              where
                              aA :: (
                                    ) => value -> m  st el  
                                                     st2 el2
                                                     st3 el3
                                                     () -}
                         let [m, st, st2, st3, el, el2, el3, value] = map mkName ["m", "st", "st2", "st3", "el", "el2", "el3", "value"]
                         in classD (cxt []) cName [m, st, el, st2, el2, st3, el3, value]
                                   [ funDep [st] [st2] , funDep [st2] [st3] 
                                   , funDep [st,st2,el2] [el] 
                                   , funDep [st,st3,el3] [el] 
                                   -- , funDep [st2,st3,el3] [el2] 
                                   ]
                                   [sigD elemAttrName
                                      (forallT []
                                               (cxt [])
                                               ( appTn arrowT 
                                                 [ varT value
                                                 , appTn (varT m) ((map varT [st, el, st2, el2, st3, el2]) ++ [conT ''()])
                                                 ] ) ) ]

                 icVVXML <- 
                        {-  instance ( AddAttrT AAttr_A value st2 el2 st3 el3
                                    ) => A_CA VXML st el st2 el2 st3 el3 value where
                              aA v = VXML $ \p -> ((), (\el -> addAttrT el (undefined :: AAttr_A)  v) . p) -}
                         let [value, st, el, st2, el2, st3, el3, v] = map mkName ["value", "st", "el", "st2", "el2", "st3", "el3", "v"]
                         in instanceD  (cxt [ appTn (conT ''T.AddAttrT) [conT nameN, varT value, varT st2, varT el2, varT st3, varT el2] ]) -- not sure about this el2, should be el3 ? 
                                       (appTn (conT cName) (conT ''VXML : map varT [st,el, st2, el2, st3, el3, value]))
                                       [ funD elemAttrName [ clause [varP v] (normalB 
                                       [| VXML $ \p -> ((), (\el -> addAttrT el $(undType $ conT nameN)  $(varE v)) . p)
                                       |]) []] ]

                 icVXMLT <- 
                        {-  instance ( Monad m'
                                  , AddAttrT AAttr_A value st2 el2 st3 el3
                                  ) => A_CA (VXMLT m') st el st2 el2 st3 el3 value where
                            aA v = VXMLT $ \p -> return ((), (\el -> addAttrT el (undefined :: AAttr_A) v) . p) -}

                         let [value, st, el, st2, el2, st3, el3, m, v] = map mkName ["value", "st", "el", "st2", "el2", "st3", "el3", "m", "v"]
                         in instanceD  (cxt [ appTn (conT ''T.AddAttrT) [conT nameN, varT value, varT st2, varT el2, varT st3, varT el2] 
                                            , appT (conT ''Monad) (varT m)])
                                       (appTn (conT cName) ((appT (conT ''VXMLT) (varT m)) : map varT [st,el, st2, el2, st3, el3, value]))
                                       [ funD elemAttrName [ clause [varP v] (normalB 
                                       [| VXMLT $ \p -> return ((), (\el -> addAttrT el $(undType $ conT nameN) $(varE v)) . p)
                                       |]) []] ]
                 return [d,s,add,aClass,icVVXML,icVXMLT] ) attrNamesUniq

        -- | elements and attribute data belonging to it
        types <- liftM concat $ mapM toCode zipped

        -- XmlIdsFromRoot instance
        docClass <- ST.lift $
          let toMaybeStr Nothing = conE 'Nothing
              toMaybeStr (Just s) = appE (conE 'Just) (litE . stringL $ s)
          in sequence [
            instanceD (cxt []) (appT (conT ''XmlIdsFromRoot) ( (conT . mkName . (dataElName ng) . fst . head) zipped) )
                [ funD 'xmlIds [ clause [wildP] (normalB (appEn (conE 'XmlIds) (map toMaybeStr [pub, sys]))) []] ]
            ]

        let all = attrDecs ++ types ++ docClass
        gs <- ST.get
        let text = unlines $    ( if showGeneratedCode then
                                      ("-- ============= generated code based on " ++ file)
                                       : map pprint all
                                       ++ [ "-- ============ generated code end =====================================" ]
                             else [] )

                             ++ if debugStates then [ "debug states: ", show (stateList gs)] else []

        when showGeneratedCode $ ST.lift $ runIO $ do -- for debugging purposes print generated code
                 hPutStrLn stdout text
                 hFlush stdout
        when writeInfoFile $ ST.lift $ runIO $ do
           let dir = "autogenerated-code"
           createDirectoryIfMissing False dir
           writeFile (dir </> (takeFileName file)) text

        return all
