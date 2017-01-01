-- Sudoku solver

import           Control.Monad
import           Data.List
import           Data.List.Split
import qualified Data.Map        as M
import           Data.Maybe
import qualified Data.Set        as S
import qualified Data.Text       as T

type Grid = M.Map String String

rows = ['A'..'I']
cols = ['1'..'9']
chars = cols ++ "0."

squares = [ x:[y] | x <- rows, y <- cols ]
units   = [ [ x:[y] | y <- cols ] | x <- rows ] ++
          [ [ x:[y] | x <- rows ] | y <- cols ] ++
          [ [ x:[y] | x <- x', y <- y' ]
                | x' <- ["ABC","DEF","GHI"], y' <- ["123","456","789"] ]
unitMap = M.fromList [ (s, filter (elem s) units) | s <- squares ]
peers   = M.fromList [ (s, plist s) | s <- squares ]
    where plist s = let p = S.fromList . concatMap id . fromJust $ M.lookup s unitMap
                    in S.difference p $ S.singleton s

create :: String -> Maybe Grid
create grid
    | length grid /= 81 = Nothing
    | not $ S.isSubsetOf (S.fromList grid) (S.fromList chars) = Nothing
    | otherwise = Just $ M.fromList (zip squares grid')
    where grid' = map (\x -> [x]) grid -- (: [])?

parseGrid :: Grid -> Maybe Grid
parseGrid grid = let empty = M.fromList $ zip squares (repeat cols)
                     givens = M.toList $ M.filter (\[x] -> x `elem` cols) grid
                 in foldM (\e (s,d) -> assign s d e) empty givens

assign :: String -> String -> Grid -> Maybe Grid
assign square value grid = foldM (flip (eliminate square)) grid others
    where others = (grid M.! square) \\ value

eliminate :: String -> Char -> Grid -> Maybe Grid
eliminate square value grid =
    if value `notElem` (grid M.! square)
    then return grid
    else do
      let values = delete value (grid M.! square)
          new_grid = M.insert square values grid
      new_grid' <- case values of
                     [] -> Nothing
                     [x] -> foldM (\v p -> eliminate p x v)
                              new_grid
                                (S.toList $ peers M.! square)
                     _ -> return new_grid
      foldM elimFn new_grid' (unitMap M.! square)
        where elimFn g unit = case filter (\u -> value `elem` g M.! u) unit of
                                [] -> Nothing
                                [x] -> assign x [value] g
                                _ -> return g

search :: Maybe Grid -> Maybe Grid
search Nothing = Nothing
search (Just grid) =
    if all ((==1) . length) $ M.elems grid
    then return grid
    else do
      let unsolved = M.map length $ M.filter ((>1) . length) grid
          unsolved_list = M.assocs unsolved
          min_unsolved = fst $ foldr (\min x -> if snd min > snd x then x else min)
                           (head unsolved_list)
                             unsolved_list
      listToMaybe $ catMaybes
        [ search (assign min_unsolved [v] grid) | v <- grid M.! min_unsolved ]

display :: Grid -> IO ()
display values = do
  let width = succ . maximum . map length $ M.elems values
      line = (intercalate "+" . replicate 3 $ replicate (width * 3) '-') ++ "\n"
      values' = map (T.center width ' ' . T.pack) $ M.elems values
      rows = chunksOf 9 values'
      rowChunks = map (chunksOf 3) rows
      blah = map (map (concatMap T.unpack)) rowChunks
      bleh = map (intercalate "|") blah
      final = intercalate line . map unlines $ chunksOf 3 bleh
  putStrLn final

main = display . fromJust . search $
         parseGrid =<< create
           "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
