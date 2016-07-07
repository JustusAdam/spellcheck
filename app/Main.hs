{-# LANGUAGE TemplateHaskell #-}


import           Control.Monad
import           Data.Foldable
import           Data.HashSet
import           Data.Maybe
import           Data.Traversable
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           System.Environment
import           Text.Read
import           Text.Spellcheck


pforest :: PrefixMap
pforest = $(do
    qAddDependentFile "words"

    f <- runIO $ readFile "words"
    return $ VarE 'mkPrefixForest `AppE` (ListE []) `AppE` (AppE (VarE 'lines) (LitE (StringL f)))
    )


allWords :: HashSet String
allWords = $(do
    f <- runIO $ readFile "words"
    return $ VarE 'fromList `AppE` (VarE 'lines `AppE` (LitE (StringL f)))
    )


checkFile :: FilePath -> FilePath -> IO ()
checkFile input output = do
    contents <- readFile input
    writeFile output . unlines =<<
        for (lines contents) (\line ->
            fmap unwords $ for (words line) $ \word ->
                if word `member` allWords
                    then return word
                    else do
                        putStrLn $ "You wrote \"" ++ word ++ "\""
                        putStrLn "Did you mean one of (press enter to see more):"
                        let matches = matchWord word pforest
                        let loop remaining = do
                                let (alternatives, rem) = splitAt 4 remaining
                                for alternatives $ \(i, alt) ->
                                    putStrLn $ show i ++ ") " ++ alt
                                l <- getLine
                                case readMaybe l of
                                    Just i -> return i
                                    _ -> loop rem
                        i <- loop $ zip [0..] matches
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
                for alternatives putStrLn
                void $ getLine




main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> interactive
        [input, output] -> checkFile input output
        _ -> putStrLn "Unexpected number of command line arguments. Expected none (for interactive) or input and output filepath for filechecking."
