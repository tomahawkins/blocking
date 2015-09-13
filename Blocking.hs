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

data Block = Block
  { team     :: Team
  , teamName :: String
  , blockers :: Int
  , gainAdvantage
  , wonPointInVolley
  , wonPointOnBlock
  , wonPointOnNextAttack :: Bool
  }

type Condition = Block -> Bool

parseBlocks :: Match -> [Block]
parseBlocks (Match teamA teamB sets) = concatMap blockOutcomes $ concat sets
  where
  blockOutcomes :: Volley -> [Block]
  blockOutcomes (Volley _ a winner) = f a
    where
    f a = case a of
      [] -> []
      [Attack side n] -> [Block
        { team                 = other side
        , teamName             = if side == A then teamB else teamA
        , blockers             = n
        , gainAdvantage        = xor side winner
        , wonPointInVolley     = xor side winner
        , wonPointOnBlock      = xor side winner
        , wonPointOnNextAttack = False
        }]
      Attack side n : a@(Attack side' _) : rest -> Block
        { team                 = other side
        , teamName             = if side == A then teamB else teamA
        , blockers             = n
        , gainAdvantage        = xor side side'
        , wonPointInVolley     = xor side winner
        , wonPointOnBlock      = False
        , wonPointOnNextAttack = null rest && xor side side' && xor side winner
        } : f (a : rest)
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
reportMatch m@(Match teamA' teamB' _) = do
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
  blocks = parseBlocks m

reportCombined :: [Match] -> IO ()
reportCombined matches = do
  report blocks "Combined Clarion advantage gained by 0 blockers" gainAdvantage (clarion `and'` b0)
  report blocks "Combined Clarion advantage gained by 1 blockers" gainAdvantage (clarion `and'` b1)
  report blocks "Combined Clarion advantage gained by 2 blockers" gainAdvantage (clarion `and'` b2)
  report blocks "Combined Clarion advantage gained by 3 blockers" gainAdvantage (clarion `and'` b3)
  putStrLn ""
  report blocks "Combined (all teams) advantage gained by 0 blockers" gainAdvantage b0
  report blocks "Combined (all teams) advantage gained by 1 blockers" gainAdvantage b1
  report blocks "Combined (all teams) advantage gained by 2 blockers" gainAdvantage b2
  report blocks "Combined (all teams) advantage gained by 3 blockers" gainAdvantage b3
  putStrLn ""
  report blocks "Combined (all teams) won point on block by 0 blockers" wonPointOnBlock b0
  report blocks "Combined (all teams) won point on block by 1 blockers" wonPointOnBlock b1
  report blocks "Combined (all teams) won point on block by 2 blockers" wonPointOnBlock b2
  report blocks "Combined (all teams) won point on block by 3 blockers" wonPointOnBlock b3
  putStrLn ""
  report blocks "Combined (all teams) won point on next attack by 0 blockers" wonPointOnNextAttack b0
  report blocks "Combined (all teams) won point on next attack by 1 blockers" wonPointOnNextAttack b1
  report blocks "Combined (all teams) won point on next attack by 2 blockers" wonPointOnNextAttack b2
  report blocks "Combined (all teams) won point on next attack by 3 blockers" wonPointOnNextAttack b3
  putStrLn ""
  report blocks "Combined (all teams) won point on block or next attack by 0 blockers" (wonPointOnBlock `or'` wonPointOnNextAttack) b0
  report blocks "Combined (all teams) won point on block or next attack by 1 blockers" (wonPointOnBlock `or'` wonPointOnNextAttack) b1
  report blocks "Combined (all teams) won point on block or next attack by 2 blockers" (wonPointOnBlock `or'` wonPointOnNextAttack) b2
  report blocks "Combined (all teams) won point on block or next attack by 3 blockers" (wonPointOnBlock `or'` wonPointOnNextAttack) b3
  putStrLn ""
  where
  blocks = concatMap parseBlocks matches

b0 = (== 0) . blockers
b1 = (== 1) . blockers
b2 = (== 2) . blockers
b3 = (== 3) . blockers
teamA = (== A) . team
teamB = (== B) . team
clarion = (== "Clarion") . teamName

and' :: Condition -> Condition -> Condition
and' a b x = a x && b x

or' :: Condition -> Condition -> Condition
or' a b x = a x || b x

percentage :: [Block] -> Condition -> Condition -> Double
percentage blocks a b = 100 * fromIntegral (length a') / fromIntegral (length b')
  where
  b' = filter b blocks
  a' = filter a b'

sampleSize :: [Block] -> Condition -> Int
sampleSize blocks a = length $ filter a blocks

