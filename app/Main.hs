{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


import           ClassyPrelude
import qualified Data.HashSet               as HSet
import           Data.List                  ((!!))
import qualified Data.List                  as List
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Text.Read
import           Text.Spellcheck


pforest :: PrefixMap
pforest = $(do
    qAddDependentFile "words"

    f <- runIO $ readFile "words"
    let wrds = VarE 'T.words `AppE` LitE (StringL f)
    return $ VarE 'mkPrefixForest `AppE` (ListE []) `AppE` wrds
    )


allWords :: HashSet Text
allWords = $(do
    f <- runIO $ readFile "words"
    let wrds = VarE 'T.words `AppE` LitE (StringL f)
    return $ VarE 'HSet.fromList `AppE` wrds
    )


checkFile :: FilePath -> FilePath -> IO ()
checkFile input output = do
    contents <- T.readFile input
    T.writeFile output . T.unlines =<<
        for (T.lines contents) (\line ->
            fmap T.unwords $ for (T.words line) $ \word ->
                if word `member` allWords
                    then return word
                    else do
                        putStrLn $ "You wrote \"" ++ word ++ "\""
                        putStrLn "Did you mean one of (press enter to see more):"
                        let matches = matchWord word pforest
                        let loop remaining = do
                                let (alternatives, rem') = List.splitAt 4 remaining
                                for_ alternatives $ \(i, alt) ->
                                    putStrLn $ pack (show i) ++ ") " ++ alt
                                l <- getLine
                                case readMaybe l of
                                    Just i -> return i
                                    _ -> loop rem'
                        i <- loop $ zip [0 :: Int ..] matches
                        return $ matches !! i)


interactive :: IO ()
interactive = forever $ do
    putStrLn "Type some words to check"
    line <- getLine
    for_ (words line) $ \w -> do
        if w `member` allWords
            then putStrLn $ w ++ " seems to be typed correctly"
            else do
                putStrLn $ "You typed " ++ w
                putStrLn "Did you mean one of:"
                let alternatives = take 4 $ matchWord w pforest
                for_ alternatives putStrLn
                void $ asText <$> getLine




main :: IO ()
main = do
    args <- map unpack <$> getArgs
    case args of
        [] -> interactive
        [input, output] -> checkFile input output
        _ -> putStrLn "Unexpected number of command line arguments. Expected none (for interactive) or input and output filepath for filechecking."
