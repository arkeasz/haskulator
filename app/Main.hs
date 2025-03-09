module Main (main) where

import Lib
import Data.Char (isDigit)

parseExpr :: String -> (Double, String)
parseExpr s = parseTerm s
  where
    parseTerm s1 = let (x, s2) = parseFactor s1 in parseTerm' x s2
    parseTerm' x ('+':s) = let (y, s') = parseFactor s in parseTerm' (x + y) s'
    parseTerm' x ('-':s) = let (y, s') = parseFactor s in parseTerm' (x - y) s'
    parseTerm' x s = (x, s)

    parseFactor s1 = let (x, s2) = parseNumber s1 in parseFactor' x s2
    parseFactor' x ('*':s) = let (y, s') = parseNumber s in parseFactor' (x * y) s'
    parseFactor' x ('/':s) = let (y, s') = parseNumber s in parseFactor' (x / y) s'
    parseFactor' x s = (x, s)

    parseNumber s = (read num, rest)
      where (num, rest) = span (\c -> isDigit c || c == '.') s

calculate :: String -> Either String Double
calculate input = case parseExpr input of
    (result, []) -> Right result
    (_, rest) -> Left ("Error: Unparsed input: " ++ rest)

main :: IO ()
main = do
    putStrLn "Welcome to Haskulator! Enter an expression to evaluate:"
    input <- getLine
    case calculate input of
        Left err -> putStrLn err
        Right result -> print result
    main
