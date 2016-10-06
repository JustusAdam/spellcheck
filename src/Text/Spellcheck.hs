{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE ViewPatterns     #-}
module Text.Spellcheck where


import           ClassyPrelude
import           Control.Monad.State.Class (MonadState)
import           Control.Monad.State.Lazy  (evalState, get, modify, put)
import qualified Data.HashMap.Strict       as HMap
import           Data.IntMap               (fromListWith)
import qualified Data.IntMap               as IMap
import           Data.List                 (nub)
import           Data.Tree


type PrefixMap = Forest (Char, Maybe Text)
data EMapValue = MkEMapValue { reachedWords :: HashSet Text, subedits :: EditMap }
newtype EditMap = MkEditMap { unEditMap :: IntMap EMapValue }


deleteCost :: Int
deleteCost = 3
insertCost :: Int
insertCost = 3


emptyEMap :: EditMap
emptyEMap = MkEditMap mempty
onlySubedits :: EditMap -> EMapValue
onlySubedits = MkEMapValue mempty
onlySubeditsFrom :: PrefixMap -> Text -> EMapValue
onlySubeditsFrom = (onlySubedits .) . mkEditMap
singleWordReached :: Text -> EMapValue
singleWordReached w = MkEMapValue (singletonSet w) emptyEMap


costReduction :: HMap.HashMap Char (HashSet Char)
costReduction = mapFromList $ map (second setFromList)
    [ ('a', "qwswx")
    , ('b', "gvhn")
    , ('c', "dfvx")
    , ('d', "efscxr")
    , ('e', "rdsw")
    , ('f', "rgdtcv")
    , ('g', "tyfhvb")
    , ('h', "yugjbn")
    , ('i', "uojk")
    , ('j', "uihknm")
    , ('k', "iojlm")
    , ('l', "opk")
    , ('m', "kjn")
    , ('n', "hjbm")
    , ('o', "plki")
    , ('p', "lo")
    , ('q', "wa")
    , ('r', "etdf")
    , ('s', "awedxz")
    , ('t', "ryfg")
    , ('u', "ijhy")
    , ('v', "fgcb")
    , ('w', "qase")
    , ('x', "zsdc")
    , ('y', "uthg")
    , ('z', "asx")
    ]


replaceCost :: Char -> Char -> Int
replaceCost c                                        ((== c) -> True)           = 0
replaceCost ((`lookup` costReduction) -> Just found) ((`member` found) -> True) = 1
replaceCost _                                        _                          = 3



mkPrefixForest :: String -> [Text] -> PrefixMap
mkPrefixForest revPrefix = map f . groupBy hasSamePrefix . sort
  where
    hasSamePrefix = (==) `on` headEx

    f words'@(x:_) = Node label subtree
      where
        label = (head', foundWord)
        subtree = mkPrefixForest (head':revPrefix) substrings
        foundWord
            | null nulls = Nothing
            | otherwise = Just $ pack $ reverse $ head':revPrefix
        head' = headEx x
        (nulls, substrings) = partition null $ map tailEx words'
    f _ = error "empty list from groupBy"


editMapCombiner :: EMapValue -> EMapValue -> EMapValue
editMapCombiner (MkEMapValue reached1 emap1) (MkEMapValue reached2 emap2) =
    MkEMapValue { reachedWords = reached1 `union` reached2
                , subedits = MkEditMap $ unionWith editMapCombiner (unEditMap emap1) (unEditMap emap2)
                }


mkCompletionMap :: PrefixMap -> EditMap
mkCompletionMap = MkEditMap . fromListWith editMapCombiner . map f
  where
    f (Node (_, word) subf) = (insertCost, MkEMapValue (maybe mempty singletonSet word) (mkCompletionMap subf))


mkEditMap :: PrefixMap -> Text -> EditMap
mkEditMap pmap s@(uncons -> Just (currChar,rest)) =
    MkEditMap $ fromListWith editMapCombiner $ pmap >>= f
  where
    f (Node (c, word) subf) =
        let weight = replaceCost currChar c
            wordToEntry w = [(weight + length rest, singleWordReached w)]
        in
            [ (weight    , onlySubeditsFrom subf rest)
            , (deleteCost, onlySubeditsFrom pmap rest)
            , (insertCost, onlySubeditsFrom subf s   )
            ]
            ++ maybe [] wordToEntry word
mkEditMap pmap _ = mkCompletionMap pmap


extractMin :: MonadState (IntMap EMapValue) m => m (Maybe (IMap.Key, EMapValue))
extractMin = do
    s <- get
    if null s
        then return Nothing
        else do
            let (i, s') = IMap.deleteFindMin s
            put s'
            return $ return i


matchWord :: Text -> PrefixMap -> [Text]
matchWord w pmap = nub $ evalState comp initialMap
  where
    initialMap = mapFromList
        [ (0       , onlySubeditsFrom pmap w)
        , (length w, singleWordReached w    )
        ]
    comp = do
        x <- extractMin
        case x of
            Just (cost, value) -> do
                let updated = IMap.mapKeys (+cost) $ unEditMap (subedits value)
                modify (unionWith editMapCombiner updated)
                fmap (toList (reachedWords value) ++) comp
            _ -> return []
