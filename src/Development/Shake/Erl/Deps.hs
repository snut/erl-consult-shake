{-# LANGUAGE OverloadedStrings #-}
module Development.Shake.Erl.Deps where

import Development.Shake.Erl.Consult
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V

import Data.Maybe (mapMaybe, catMaybes)
import Data.Either (rights)
import Data.List (partition)
import System.FilePath
import System.Directory (doesFileExist)
import Control.Arrow (first, (***))

import Data.Aeson
import qualified Data.HashMap.Strict as H

data Deps = Deps
  { app :: String
  , backTransitive :: Set (String, Source)
  , deps :: Map String Dep
  } deriving (Eq, Show)

data Dep = Dep
  { primary :: Maybe Source
  , transitive :: Set (String, Source)
  } deriving (Eq, Show)

data Source = Source
  { url :: Text
  , version :: Version
  } deriving (Eq, Show, Ord)

data Version = Tag Text | Branch Text | Commit Text | UnknownVersion deriving (Eq, Show, Read, Ord)

directDeps :: String -> [ErlTerm] -> Deps
directDeps name ts = Deps name S.empty $ depList ts M.empty
  where
    depList :: [ErlTerm] -> Map String Dep -> Map String Dep
    depList (ErlTuple [ErlAtom dep, ErlString _, ErlTuple [ErlAtom "git", ErlString url, vsn]] : ts) = depList ts . ins
      where ins = M.insert dep (Dep (Just prim) S.empty)
            prim = Source url (version vsn)
    depList (_ : ds) = depList ds
    depList [] = id

    version :: ErlTerm -> Version
    version (ErlTuple [ErlAtom "tag", ErlString tag]) = Tag tag
    version (ErlTuple [ErlAtom "branch", ErlString branch]) = Branch branch
    version _ = UnknownVersion
--directDeps _ = Deps $ M.empty

direct :: String -> [ErlTerm] -> Maybe Deps
direct name (ErlTuple [ErlAtom "deps", ErlList xs] : _) = Just $ directDeps name xs
direct name (_ : ts) = direct name ts
direct _    [] = Nothing

maybeInsert :: Ord a => Maybe a -> Set a -> Set a
maybeInsert Nothing  s = s
maybeInsert (Just x) s = S.insert x s

depToSet :: String -> Dep -> Set (String, Source)
depToSet name (Dep pri tra) = maybeInsert ((,) name <$> pri) tra

-- dirty hack for M.insertWith type-tetris:
includeTransitive :: Dep -> Dep -> Dep
includeTransitive dep more = dep { transitive = S.union (transitive dep) (transitive more) }

-- | In `addTransitive primary secondary`, introduce everything in `secondary` as a transitive dependency in `primary`
addTransitive :: Deps -> Deps -> Deps
addTransitive primary secondary = primary { backTransitive = backTransitive', deps = deps' }
  where
    deps' = foldr (uncurry (M.insertWith includeTransitive)) (deps primary) tranEmptySource
    tranEmptySource = map (\(k, d) -> (k, Dep Nothing (depToSet sname d))) tranDeps    
    backTransitive' = foldl S.union (backTransitive primary) (map (depToSet sname . snd) backDeps)
    (backDeps, tranDeps) = partition ((== app primary) . fst) slist
    slist = M.toList (deps secondary)
    sname = app secondary
  

maybeConsultFile :: FilePath -> IO ( Maybe [ErlTerm] )
maybeConsultFile file = do
  exists <- doesFileExist file
  if exists
    then do
      cslt <- consultFile file
      case cslt of
        Right x -> return $ Just x
        Left _  -> return Nothing
    else return Nothing
        
includeTransitiveDeps :: FilePath -> Deps -> IO Deps
includeTransitiveDeps libPath primary = do
  let names = M.keys (deps primary)
  ds <- mapM (\name -> fmap ((,) name) <$> maybeConsultFile (libPath </> name </> "rebar.config")) names
  let secondaries = mapMaybe (uncurry direct) $ catMaybes ds
  return $ foldl addTransitive primary secondaries

fixDeps :: Int -> FilePath -> Deps -> IO Deps
fixDeps 0 _    deps = putStrLn "Failed to find dependency fixpoint" >> return deps
fixDeps n path deps = do
  deps' <- includeTransitiveDeps path deps
  if deps' == deps
    then return deps
    else fixDeps (n-1) path deps'

getDeps :: FilePath -> String -> IO (Maybe Deps)
getDeps path app = do
  primary <- consultFile rebar
  case primary of
   Right terms -> traverse (fixDeps 8 lib) (direct app terms)
   Left  _     -> return Nothing
  where
    lib   = path </> app </> "lib"
    rebar = path </> app </> "rebar.config"

------

instance ToJSON Deps where
  toJSON (Deps app back dps) = Object $ H.fromList [ ("app", toJSON app)
                                                   , ("back", toJSON back)
                                                   , ("deps", toJSON dps) ]

instance ToJSON Dep where
  toJSON (Dep pr t) = Object $ H.fromList [ ("primary", maybe (toJSON $ Text.pack "?") toJSON pr)
                                          , ("transitive", Array $ V.fromList transitiveJSON) ]
    where
      transitiveList = S.toList t
      transitiveJSON = map (uncurry (inject "source") . (toJSON *** toJSON)) transitiveList

instance ToJSON Source where
  toJSON (Source url version) = Object $ H.fromList [("url", toJSON url), ("version", toJSON version)]

instance ToJSON Version where
  toJSON (Tag t) = Object $ H.singleton "tag" (toJSON t)
  toJSON (Branch b) = Object $ H.singleton "branch" (toJSON b)
  toJSON (Commit h) = Object $ H.singleton "commit" (toJSON h)
  toJSON _ = Object $ H.singleton "unknown" (toJSON $ Text.pack "?")


inject :: Text -> Value -> Value -> Value
inject k v (Object hm) = Object $ H.insert k v hm
inject _ _ v = v
