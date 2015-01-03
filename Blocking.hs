module Main (main) where

import Data.List
import System.Environment
import Text.Printf

import Match

type Condition = Block -> Bool

main :: IO ()
main = do
  files <- getArgs
  blocks <- mapM readFile files >>= return . map parseBlocks
  let report' = report (blocks ++ [concat blocks])
  report' "Combined advantage gained by 0 blockers      " gainAdvantage b0
  report' "Combined advantage gained by 1 blockers      " gainAdvantage b1
  report' "Combined advantage gained by 2 blockers      " gainAdvantage b2
  putStrLn ""
  report' "Team A   advantage gained by 0 blockers      " gainAdvantage (b0  `and'` teamA)
  report' "Team A   advantage gained by 1 blockers      " gainAdvantage (b1  `and'` teamA)
  report' "Team A   advantage gained by 2 blockers      " gainAdvantage (b2  `and'` teamA)
  putStrLn ""
  report' "Team B   advantage gained by 0 blockers      " gainAdvantage (b0  `and'` teamB)
  report' "Team B   advantage gained by 1 blockers      " gainAdvantage (b1  `and'` teamB)
  report' "Team B   advantage gained by 2 blockers      " gainAdvantage (b2  `and'` teamB)
  putStrLn ""

swapTeams :: Match -> Match
swapTeams a = case a of
  a : b : c -> a : set b : swapTeams c
  a -> a
  where
  set :: Set -> Set
  set (Set a b) = Set (map volley a) $ team b

  volley :: Volley -> Volley
  volley (Volley a b) = Volley (team a) $ map attack b

  attack :: Attack -> Attack
  attack (Attack a b) = Attack (team a) b

  team :: Team -> Team
  team A = B
  team B = A

data Volley' = Volley' Team [Attack] Team deriving Show

parseVolleys' :: String -> [Volley']
parseVolleys' = concatMap volleys' . swapTeams . parseMatch
  where
  volleys' :: Set -> [Volley']
  volleys' (Set a w) = f a
    where
    f :: [Volley] -> [Volley']
    f a = case a of
      [] -> []
      Volley a b : []                    -> Volley' a b w : []
      Volley a b : d@(Volley c _) : rest -> Volley' a b c : f (d : rest)

data Block = Block Team Int Bool

parseBlocks :: String -> [Block]
parseBlocks = concatMap blockOutcomes . parseVolleys'

blockOutcomes :: Volley' -> [Block]
blockOutcomes (Volley' _ a winner) = f a
  where
  f a = case a of
    [] -> []
    [Attack side n] -> [Block (other side) n $ xor side winner]
    Attack side n : a@(Attack side' _) : rest -> Block (other side) n (xor side side') : f (a : rest)
  other a = case a of
    A -> B
    B -> A
  xor a b = case (a, b) of
    (A, A) -> False
    (B, B) -> False
    _ -> True

report :: [[Block]] -> String -> Condition -> Condition -> IO ()
report blocks msg a b = putStrLn $ msg ++ " : " ++ intercalate " " [ printf "%3.0f%% (%3d)" (percentage blocks a b) (sampleSize blocks b) | blocks <- blocks ]

gainAdvantage (Block _ _ a) = a
b0  (Block _ a _) = a == 0
b1  (Block _ a _) = a == 1
b2  (Block _ a _) = a == 2
b01 (Block _ a _) = a == 0 || a == 1
b12 (Block _ a _) = a == 1 || a == 2
teamA (Block a _ _) = a == A
teamB (Block a _ _) = a == B

and' :: Condition -> Condition -> Condition
and' a b x = a x && b x

percentage :: [Block] -> Condition -> Condition -> Double
percentage blocks a b = 100 * fromIntegral (length a') / fromIntegral (length b')
  where
  b' = filter b blocks
  a' = filter a b'

sampleSize :: [Block] -> Condition -> Int
sampleSize blocks a = length $ filter a blocks

