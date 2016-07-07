{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Text.Spellcheck where


import           ClassyPrelude
import           Control.Lens
import           Control.Monad.State.Lazy (evalState, get, modify, put)
import           Control.Monad.Writer
import qualified Data.HashMap.Strict      as HMap
import qualified Data.HashSet             as HSet
import           Data.IntMap              (findMin, fromListWith)
import qualified Data.IntMap              as IMap
import           Data.Tree


type PrefixMap = Forest (Char, Maybe String)
newtype EditMap = MkEditMap { unEditMap :: IntMap ([String], EditMap) }


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



mkPrefixForest :: String -> [String] -> PrefixMap
mkPrefixForest revPrefix = map f . groupBy hasSamePrefix . sort
  where
    hasSamePrefix (x:_) (y:_) = x == y
    hasSamePrefix [] [] = error "unexpected empty list"
    hasSamePrefix _ _ = False

    f words@((head:_):_) = Node (head, if null nulls then Nothing else Just (reverse (head:revPrefix))) $ mkPrefixForest (head:revPrefix) substrings
      where
        (nulls, substrings) = partition null $ map tailEx words
    f _ = error "empty list from groupBy"


editMapCombiner (str1, emap1) (str2, emap2) = (str1 ++ str2, MkEditMap $ unionWith editMapCombiner (unEditMap emap1) (unEditMap emap2))


mkCompletionMap :: PrefixMap -> EditMap
mkCompletionMap = MkEditMap . fromListWith editMapCombiner . map f
  where
    f (Node (_, word) subf) = (insertCost, (maybe [] return word, mkCompletionMap subf))


mkEditMap :: PrefixMap -> String -> EditMap
mkEditMap pmap [] = mkCompletionMap pmap
mkEditMap pmap s@(currChar:rest) = emap
  where
    emap = MkEditMap $ fromListWith editMapCombiner $ pmap >>= f
    f (Node (c, word) subf) =
        let weight = if c == currChar then 0 else replaceCost currChar c
        in
            [ (weight, ([], mkEditMap subf rest))
            , (deleteCost, ([], mkEditMap pmap rest))
            , (insertCost, ([], mkEditMap subf s))
            ]
            ++ maybe [] (\w -> [(weight + length rest, ([w], MkEditMap $ IMap.empty))]) word


extractMin = do
    s <- get
    let (i, s') = IMap.deleteFindMin s
    put s'
    return i


matchWord :: String -> PrefixMap -> [String]
matchWord w pmap = evalState comp (IMap.fromList [(0, ([], mkEditMap pmap w)), (length w, ([w], MkEditMap IMap.empty))])
  where
    comp = do
        (cost, (words, innerMap)) <- extractMin
        let updated = IMap.mapKeys (+cost) $ unEditMap innerMap
        modify (unionWith editMapCombiner updated)
        fmap (words ++) comp
