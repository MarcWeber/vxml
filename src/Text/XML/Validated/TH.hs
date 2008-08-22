{-# LANGUAGE TemplateHaskell #-}
module Text.XML.Validated.TH where
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

toCode ::  ( String, (Maybe ElementDecl, Maybe AttListDecl)) -> Q [Dec]
toCode (n, (Just (ElementDecl _ content), Just (AttListDecl _ attdefList) ) ) = do
  let attrDataName = mkName $ (++ "_A") $ fstUpper n
      tagDataName  =  mkName $  (++ "_T") $ fstUpper n

  sequence [
    -- dataAttribute 
    dataD (cxt []) attrDataName [] [] []

    , -- instanceAttrType
    instanceOfSimple (conT ''AttributeType) (conT attrDataName)
    -- instanceAttrShow <- instanceShow attrDataName n

    , -- dataTag
    dataD (cxt []) tagDataName [] [] []

    , -- instanceTagType
    instanceOfSimple (conT ''TagType) (conT tagDataName)

    , -- function tagname for convinience 
    funD (mkName $ fstLower n) 
         [ clause [] (normalB (appEn (varE 'createTagT) [sigE (varE 'undefined) (conT tagDataName)] ) ) []]

    , -- instanceTagShow
    instanceShow tagDataName n
    -- instanceTagInitialState
    ]



dtdToTypes :: FilePath -> Q [Dec]
dtdToTypes file = do
  read <- runIO $ liftM (dtdParse' file) $ readFile file
  case read of
    Left a -> fail a
    Right (Just (DTD name mbExtId decls))  -> do
    types <- liftM concat $ mapM toCode $ zipElements decls
    runIO $ do -- for debugging purposes print generated code
       putStrLn "============= generated code ========================================="
       mapM_ (putStrLn . pprint) types
       putStrLn "============= generated code end ====================================="
       hFlush stdout
    return types
