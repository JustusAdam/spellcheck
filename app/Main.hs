{-# LANGUAGE TemplateHaskell   #-}


import           ClassyPrelude
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Text.Spellcheck


pforest :: PrefixMap
pforest = mkPrefixForest [] $ toList allWords


allWords :: HashSet Text
allWords = setFromList $ words $ $(do
    qAddDependentFile "words"
    LitE . StringL <$> runIO (readFile "words")
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
                                let (alternatives, rem') = splitAt 4 remaining
                                for_ alternatives $ \(i, alt) ->
                                    putStrLn $ pack (show i) ++ ") " ++ alt
                                when (null alternatives) $ putStrLn "Sorry, no more alternatives found."
                                l <- getLine
                                case readMay (l :: Text) of
                                    Just i -> return i
                                    _ -> loop rem'
                        i <- loop $ zip [0 :: Int ..] matches
                        return $ matches `indexEx` i)


interactive :: IO ()
interactive = forever $ do
    putStrLn "Type some words to check"
    line <- getLine
    for_ (words line) $ \w ->
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
