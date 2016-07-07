{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
module Text.Spellcheck where


import           ClassyPrelude
import           Control.Monad.State.Class (MonadState)
import           Control.Monad.State.Lazy  (evalState, get, modify, put)
import           Control.Monad.Writer
import qualified Data.HashMap.Strict       as HMap
import qualified Data.HashSet              as HSet
import           Data.IntMap               (findMin, fromListWith)
import qualified Data.IntMap               as IMap
import qualified Data.Text                 as T
import           Data.Tree


type PrefixMap = Forest (Char, Maybe Text)
type EMapValue = (HashSet Text, EditMap)
newtype EditMap = MkEditMap { unEditMap :: IntMap EMapValue }


deleteCost :: Int
deleteCost = 3
insertCost :: Int
insertCost = 3


costReduction :: HMap.HashMap Char (HashSet Char)
costReduction = HMap.fromList $ map (second HSet.fromList)
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
replaceCost typed expected =
    case HSet.member expected <$> HMap.lookup typed costReduction of
        Just True -> 1
        Nothing -> 3



mkPrefixForest :: String -> [Text] -> PrefixMap
mkPrefixForest revPrefix = map f . groupBy hasSamePrefix . sort
  where
    hasSamePrefix x y =  T.head x == T.head y

    f words@(x:_) = Node (head, if null nulls then Nothing else Just (T.pack $ reverse $ head:revPrefix)) $ mkPrefixForest (head:revPrefix) substrings
      where
        head = T.head x
        (nulls, substrings) = partition T.null $ map tailEx words
    f _ = error "empty list from groupBy"


editMapCombiner ::EMapValue -> EMapValue -> EMapValue
editMapCombiner (str1, emap1) (str2, emap2) = (str1 `HSet.union` str2, MkEditMap $ unionWith editMapCombiner (unEditMap emap1) (unEditMap emap2))


mkCompletionMap :: PrefixMap -> EditMap
mkCompletionMap = MkEditMap . fromListWith editMapCombiner . map f
  where
    f (Node (_, word) subf) = (insertCost, (maybe HSet.empty HSet.singleton word, mkCompletionMap subf))


mkEditMap :: PrefixMap -> Text -> EditMap
mkEditMap pmap s =
    case T.uncons s of
        Nothing -> mkCompletionMap pmap
        Just (currChar,rest) -> MkEditMap $ fromListWith editMapCombiner $ pmap >>= f
          where
            f (Node (c, word) subf) =
                let weight = if c == currChar then 0 else replaceCost currChar c
                in
                    [ (weight, (HSet.empty, mkEditMap subf rest))
                    , (deleteCost, (HSet.empty, mkEditMap pmap rest))
                    , (insertCost, (HSet.empty, mkEditMap subf s))
                    ]
                    ++ maybe [] (\w -> [(weight + length rest, (HSet.singleton w, MkEditMap $ IMap.empty))]) word


extractMin :: MonadState (IntMap EMapValue) m => m (IMap.Key, EMapValue)
extractMin = do
    s <- get
    let (i, s') = IMap.deleteFindMin s
    put s'
    return i


matchWord :: Text -> PrefixMap -> [Text]
matchWord w pmap = evalState comp (IMap.fromList [(0, (HSet.empty, mkEditMap pmap w)), (T.length w, (HSet.singleton w, MkEditMap IMap.empty))])
  where
    comp = do
        (cost, (words, innerMap)) <- extractMin
        let updated = IMap.mapKeys (+cost) $ unEditMap innerMap
        modify (unionWith editMapCombiner updated)
        fmap (HSet.toList words ++) comp
