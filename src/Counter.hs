module Counter where

import Data.Char
import System.Environment
import System.IO

main :: IO ()
main = do
    args <- getArgs
    mapM_ incFile args


tryRead :: (Read a) => String -> Maybe a
tryRead str = case reads str of
    [(val, "")] -> Just val
    _ -> Nothing

readNum :: FilePath -> IO Integer
readNum file = withFile file ReadMode $ \handle -> do
    contents <- hGetContents handle
    let digits = takeWhile isDigit $ dropWhile isSpace contents
    seq (length digits) $ return $ case tryRead digits of
        Nothing -> 0
        Just num -> num

writeNum :: FilePath -> Integer -> IO ()
writeNum file num = withFile file WriteMode $ \handle -> do
    hPutStr handle $ show num

incFile :: FilePath -> IO ()
incFile file = do
    num <- readNum file
    print num
    writeNum file $ num + 1


