module Sudoku where

import           Control.Monad
import           Data.List       (delete, intercalate, (\\))
import           Data.List.Split (chunksOf)
import qualified Data.Map        as M
import           Data.Maybe
import qualified Data.Set        as S
import qualified Data.Text       as T

newtype Grid = Grid { unGrid :: M.Map String String }

toGrid :: M.Map String String -> Grid
toGrid = Grid

showGrid :: Grid -> String
showGrid (Grid grid) = do
  let width = succ . maximum . map length $ M.elems grid
      line = (intercalate "+" . replicate 3 $ replicate (width * 3) '-') ++ "\n"
      text_values = map (T.center width ' ' . T.pack) $ M.elems grid
      rows = chunksOf 9 text_values
      box_rows = map (chunksOf 3) rows
      pretty_rows = map (intercalate "|" . map (concatMap T.unpack)) box_rows
  intercalate line . map unlines $ chunksOf 3 pretty_rows

instance Show Grid where
    show = showGrid

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
    | otherwise = return . Grid $ M.fromList (zip squares grid')
    where grid' = map (\x -> [x]) grid -- (: [])?

parseGrid :: Grid -> Maybe Grid
parseGrid (Grid grid) =
    let empty = M.fromList $ zip squares (repeat cols)
        givens = M.toList
                   $ M.filter (\[x] -> x `elem` cols) grid
    in foldM (\e (s,d) -> assign s d e) (toGrid empty) givens

assign :: String -> String -> Grid -> Maybe Grid
assign square value grid = foldM (flip (eliminate square)) grid others
    where others = (unGrid grid M.! square) \\ value

eliminate :: String -> Char -> Grid -> Maybe Grid
eliminate square value _grid@(Grid grid) =
    if value `notElem` (grid M.! square)
    then return _grid
    else do
      let values = delete value (grid M.! square)
          new_grid = toGrid $ M.insert square values grid
      new_grid' <- case values of
                     [] -> Nothing
                     [x] -> foldM (\v p -> eliminate p x v)
                                  new_grid
                                  (S.toList $ peers M.! square)
                     _ -> return new_grid
      foldM elimFn new_grid' (unitMap M.! square)
        where elimFn _g@(Grid g) unit =
                  case filter (\u -> value `elem` g M.! u) unit of
                    [] -> Nothing
                    [x] -> assign x [value] _g
                    _ -> return _g

search :: Maybe Grid -> Maybe Grid
search Nothing = Nothing
search (Just _grid@(Grid grid)) =
    if all ((==1) . length) $ M.elems grid
    then return _grid
    else do
      let unsolved = M.map length $ M.filter ((>1) . length) grid
          unsolved_list = M.assocs unsolved
          (min_unsolved, _) = foldr (\min x -> if snd min > snd x
                                               then x
                                               else min)
                                    (head unsolved_list)
                                    unsolved_list
      listToMaybe $ catMaybes
        [ search (assign min_unsolved [v] _grid) | v <- grid M.! min_unsolved ]

solve :: String -> IO ()
solve grid = putStrLn . maybe "no solution" show . search
               $ parseGrid =<< create grid

main = solve "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
