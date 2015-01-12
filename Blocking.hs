module Main (main) where

import Data.List
import System.Environment
import Text.Printf

import Match
import Touches

type Condition = Block -> Bool

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> return ()
    "-t" : files -> do
      attacks <- mapM readFile files >>= return . concatMap parseTouches
      let percent a b = 100 * fromIntegral (length $ filter a $ filter b attacks) / fromIntegral (length $ filter b attacks)
          sampleSize a = length $ filter a attacks
          advantage (_, a) = a
          touched   (a, _) = a
          missed    (a, _) = not a
          report msg a b = printf "%s  %3.0f%%  (%d)\n" msg (percent a b :: Double) (sampleSize b)
      report "Advantage of all blocks    " advantage (const True) 
      report "Advantage of touched blocks" advantage touched
      report "Advantage of missed  blocks" advantage missed
    files -> do
      blocks <- mapM readFile files >>= return . map parseBlocks
      let report'  = report blocks
      let report'' = report [concat blocks]
      report' "Team A   advantage gained by 0 blockers      " gainAdvantage (b0  `and'` teamA)
      report' "Team A   advantage gained by 1 blockers      " gainAdvantage (b1  `and'` teamA)
      report' "Team A   advantage gained by 2 blockers      " gainAdvantage (b2  `and'` teamA)
      report' "Team A   advantage gained by 3 blockers      " gainAdvantage (b3  `and'` teamA)
      putStrLn ""
      report' "Team B   advantage gained by 0 blockers      " gainAdvantage (b0  `and'` teamB)
      report' "Team B   advantage gained by 1 blockers      " gainAdvantage (b1  `and'` teamB)
      report' "Team B   advantage gained by 2 blockers      " gainAdvantage (b2  `and'` teamB)
      report' "Team B   advantage gained by 3 blockers      " gainAdvantage (b3  `and'` teamB)
      putStrLn ""
      report'' "Combined advantage gained by 0 blockers      " gainAdvantage b0
      report'' "Combined advantage gained by 1 blockers      " gainAdvantage b1
      report'' "Combined advantage gained by 2 blockers      " gainAdvantage b2
      report'' "Combined advantage gained by 3 blockers      " gainAdvantage b3
      putStrLn ""

data Block = Block Team Int Bool

parseBlocks :: String -> [Block]
parseBlocks = concatMap blockOutcomes . concat . parseMatch

blockOutcomes :: Volley -> [Block]
blockOutcomes (Volley _ a winner) = f a
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
b3  (Block _ a _) = a == 3
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

