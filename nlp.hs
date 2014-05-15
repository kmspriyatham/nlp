import Data.Array (listArray, (!))
import Data.List (foldr1, foldl', intercalate, map)
import Data.Map (Map, empty, insertWith, size, map, toList, findMin)

minEditDist :: String -> String -> Int
-- Computes Levenshtein minimum edit distance between two words
minEditDist x y = arr ! (lx, ly)
                    where lx = length x
                          ly = length y
                          arr = listArray ((0, 0), (lx, ly)) (Data.List.map f [(a, b) | a <- [0..lx], b <- [0..ly]])
                          f (a, b) = if a == 0
                                        then b
                                        else if b == 0
                                            then a
                                            else foldr1 min [(arr ! (a - 1, b)) + 1, (arr ! (a, b - 1)) + 1, (arr ! (a - 1, b - 1)) + g a b]
                          g a b = if x !! (a - 1) == y !! (b - 1)
                                    then 0
                                    else 2

tokens = words --NOTE: tokenization is not perfect, will be changed later.

ngrams :: Int -> String ->[[String]]

ngrams n x = let tx = tokens x in
             ngrams' tx
             where ngrams' y = if length y <= n
                                  then [y]
                                  else (take n y):ngrams' (tail y)

ngramModel :: Int -> String -> Map [String] Double

ngramModel n x = Data.Map.map (\x -> logBase 10 (fromIntegral x) - lsmx) mx
                 where lsmx = logBase 10 (fromIntegral $ length tx)
                       mx = foldl' (\x y -> insertWith (+) y 1 x) empty tx
                       tx = ngrams n x

unigrams = ngrams 1
bigrams = ngrams 2

unigramModel = ngramModel 1
bigramModel = ngramModel 2

ngramLength = length.fst.findMin

arpaFormat n fi fo = do
  s <- readFile fi
  let ngramModels = Data.List.map (flip ngramModel s) [1..n]
  writeFile fo "\\data\\\n"
  mapM_ (\x -> appendFile fo (ngramCountsFormat x ++ (show $ size $ ngramModels !! (x - 1)) ++ "\n")) [1..n]
  appendFile fo "\n"
  mapM_ (\x -> ngramModelFormat fo (ngramLength x) x) ngramModels
  appendFile fo "\\end\\"

dataLinesFormat (ss, d) = show d ++ " " ++ intercalate " " ss ++ "\n"
ngramCountsFormat n = "ngram " ++ show n ++ "="
ngramModelFormat fo n m = do
                        appendFile fo ("\\" ++ show n ++ "-grams:\\\n")
                        mapM_ (\x -> appendFile fo (dataLinesFormat x)) (toList m)
                        appendFile fo "\n"