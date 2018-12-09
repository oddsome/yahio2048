module Main where

import System.IO

import System.Random
import Data.Array.IO
import Data.List (foldl')
import System.Environment
import System.Console.ANSI

newtype Cell = Cell (Maybe Int)

instance Show Cell where
  show (Cell Nothing) = "[    ]"
  show (Cell (Just n)) = let
      numView = show (2 ^ n :: Int)
      spaces = repeat ' '
    in
      "[" ++ take (4 - length numView) spaces ++ numView ++ "]" 

defaultCell :: Cell
defaultCell = Cell Nothing

type Field  = IOArray (Int, Int) Cell

mkField :: Int -> Int -> Cell -> IO Field
mkField cols rows def = newArray ((1, 1), (cols, rows)) def

testField :: IO Field
testField = do
  f <- newArray ((1, 1), (4, 4)) defaultCell
  writeArray f (2,2) (Cell (Just 2))
  writeArray f (2,3) (Cell (Just 2))
  writeArray f (3,2) (Cell (Just 2))
  writeArray f (3,3) (Cell (Just 2))
  pure f

data Direction = U | D | L | R

type Movement = (Int, Int) -> (Int, Int)

dir2Move :: Direction -> Movement
dir2Move U = fmap pred
dir2Move D = fmap succ
dir2Move L = \(x, y) -> (pred x, y)
dir2Move R = \(x, y) -> (succ x, y)

-- moveRec :: Field -> Movement -> (Int, Int) -> IO Bool
-- moveRec f mv i = do
--   bnd <- getBounds f
--   pure False

printField :: Field -> IO ()
printField f = do
  ((minX,minY), (maxX, maxY)) <- getBounds f
  mapM_
    (\i@(_x, _y) -> do
      e <- readArray f i
      if _x == maxX
        then putStrLn (show e)
        else putStr (show e)
    )
    [(x,y) | y <- [minY .. maxY], x <- [minX .. maxX]]

dirIndices :: Direction -> (Int, Int) -> (Int, Int) -> [[(Int, Int)]]
dirIndices L (cf, rf) (ct, rt) = [[(col, row) | col <- [ct, pred ct .. cf]] | row <- [rf .. rt]]
dirIndices R (cf, rf) (ct, rt) = [[(col, row) | col <- [cf .. ct]] | row <- [rf .. rt]]
dirIndices U (cf, rf) (ct, rt) = [[(col, row) | row <- [rt, pred rt .. rf]] | col <- [cf .. ct]]
dirIndices D (cf, rf) (ct, rt) = [[(col, row) | row <- [rf .. rt]] | col <- [cf .. ct]]

fall :: [Cell] -> [Cell]
fall = (\(a,b,c) -> a ++ b ++ c) . foldl' go ([], [], []) . reverse  where
  go (s, m, u) (Cell Nothing) = (defaultCell : s, m, u)
  go (s, [], u) e
    = (s, [e], u)
  go (s, Cell h : m, u) (Cell c)
    | c == h    = (defaultCell : s, [], Cell (fmap succ h) : m ++ u)
    | otherwise = (s, Cell c : Cell h : m, u)

snap :: Direction -> Field -> IO Field
snap d f = do
  ind <- getBounds f
  let indices = uncurry (dirIndices d) ind

  pre <- sequence ( map
      (sequence . map
        (\i -> readArray f i >>= (\v -> pure (i, v)))
      )
      indices
    )

  let fallen = map ((uncurry zip) . (fmap fall) . unzip) pre

  _ <- sequence (
      map
        (sequence . map (\(i,v) -> writeArray f i v))
        fallen
    )
  pure f

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False

    args <- getArgs
    case args of
      [x, y] -> do
        f <- mkField (read x) (read y) defaultCell
        g <- getStdGen
        loop f g (read x) (read y)
      _      -> putStrLn "Wrong usage: app width height"

  where
    loop field g x y = do
      x' <- getStdRandom (randomR ((1, x) :: (Int, Int)))
      y' <- getStdRandom (randomR ((1, y) :: (Int, Int)))
      p <- readArray field (x', y')
      case p of
        Cell Nothing -> do
          choice <- getStdRandom (randomR ((1, 2) :: (Int, Int)))
          writeArray field (x', y') (Cell (Just choice))
          printField field
          c <- getChar
          continue c field (\f' -> cursorUpLine y >> loop f' g x y)
        _ -> loop field g x y

    continue 'w' field c = snap U field >>= c
    continue 's' field c = snap D field >>= c
    continue 'a' field c = snap L field >>= c
    continue 'd' field c = snap R field >>= c
    continue _   _     _ = pure ()
