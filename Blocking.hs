module Main (main) where

import System.Environment
import Text.Printf

import Match
import Touches

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
      matches <- flip mapM files $ \ f -> readFile f >>= return . parseMatch
      mapM_ reportMatch matches
      reportCombined matches

data Block = Block Team Int Bool Bool  -- Blocking team, number of blockers, if the block gained the advantage, attack on 2.
type Condition = Block -> Bool

parseBlocks :: [Set] -> [Block]
parseBlocks sets = concatMap blockOutcomes $ concat sets

blockOutcomes :: Volley -> [Block]
blockOutcomes (Volley _ a winner) = f a
  where
  f a = case a of
    [] -> []
    [Attack side n d] -> [Block (other side) n (xor side winner) d]
    Attack side n d : a@(Attack side' _ _) : rest -> Block (other side) n (xor side side') d : f (a : rest)
  other a = case a of
    A -> B
    B -> A
  xor a b = case (a, b) of
    (A, A) -> False
    (B, B) -> False
    _ -> True

report :: [Block] -> String -> Condition -> Condition -> IO ()
report blocks msg a b = printf "%s : %3.0f%% (%3d)\n" msg (percentage blocks a b) (sampleSize blocks b)

reportMatch :: Match -> IO ()
reportMatch (Match teamA' teamB' sets) = do
  printf "%s vs %s\n" teamA' teamB'
  report blocks (printf "%-10s advantage gained by 0 blockers" teamA') gainAdvantage (b0  `and'` teamA)
  report blocks (printf "%-10s advantage gained by 1 blockers" teamA') gainAdvantage (b1  `and'` teamA)
  report blocks (printf "%-10s advantage gained by 2 blockers" teamA') gainAdvantage (b2  `and'` teamA)
  report blocks (printf "%-10s advantage gained by 3 blockers" teamA') gainAdvantage (b3  `and'` teamA)
  report blocks (printf "%-10s advantage gained by 0 blockers" teamB') gainAdvantage (b0  `and'` teamB)
  report blocks (printf "%-10s advantage gained by 1 blockers" teamB') gainAdvantage (b1  `and'` teamB)
  report blocks (printf "%-10s advantage gained by 2 blockers" teamB') gainAdvantage (b2  `and'` teamB)
  report blocks (printf "%-10s advantage gained by 3 blockers" teamB') gainAdvantage (b3  `and'` teamB)
  putStrLn ""
  where
  blocks = parseBlocks sets

reportCombined :: [Match] -> IO ()
reportCombined matches = do
  report blocks "Combined advantage gained by 0 blockers" gainAdvantage b0
  report blocks "Combined advantage gained by 1 blockers" gainAdvantage b1
  report blocks "Combined advantage gained by 2 blockers" gainAdvantage b2
  report blocks "Combined advantage gained by 3 blockers" gainAdvantage b3
  putStrLn ""
  where
  blocks = parseBlocks $ concat [ sets | Match _ _ sets <- matches ]

gainAdvantage (Block _ _ a _) = a
b0            (Block _ a _ _) = a == 0
b1            (Block _ a _ _) = a == 1
b2            (Block _ a _ _) = a == 2
b3            (Block _ a _ _) = a == 3
teamA         (Block a _ _ _) = a == A
teamB         (Block a _ _ _) = a == B
--attackOn2     (Block _ _ _ a) = a
--offenseGainsAdvantage (Block _ _ a _) = not a

and' :: Condition -> Condition -> Condition
and' a b x = a x && b x

percentage :: [Block] -> Condition -> Condition -> Double
percentage blocks a b = 100 * fromIntegral (length a') / fromIntegral (length b')
  where
  b' = filter b blocks
  a' = filter a b'

sampleSize :: [Block] -> Condition -> Int
sampleSize blocks a = length $ filter a blocks

