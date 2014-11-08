module Main (main) where

import Data.List
import System.Environment
import Text.Printf

data Block
  = NoBlock
  | BlockTouch Int
  | BlockMiss  Int
  deriving Show

data Result
  = WinPoint
  | LostPoint
  | DefenseAttack
  | DefenseFreeBall
  | OffenseAttack
  | OffenseFreeBall
  deriving Show

type Attack = (Block, Result)

type Condition = Attack -> Bool

main :: IO ()
main = do
  args <- getArgs
  attacks <- mapM readFile args >>= return . map parseMatch
  let report' = report attacks
  report' "Blocks touched   of all blocks      " blockTouched  blockAttempt
  putStrLn ""
  report' "Win point        of all blocks      " winPoint      blockAttempt
  report' "Lost point       of all blocks      " lostPoint     blockAttempt
  putStrLn ""
  report' "Win point        of blocks touched  " winPoint      blockTouched
  report' "Lost point       of blocks touched  " lostPoint     blockTouched
  putStrLn ""
  report' "Win point        of blocks missed   " winPoint      blockMissed
  report' "Lost point       of blocks missed   " lostPoint     blockMissed
  putStrLn ""
  report' "Gained advantage of all blocks      " gainAdvantage blockAttempt
  report' "Gained advantage of blocks touched  " gainAdvantage blockTouched
  report' "Gained advantage of blocks missed   " gainAdvantage blockMissed
  putStrLn ""
  report' "Gained advantage of with no blockers" gainAdvantage noBlockers
  report' "Gained advantage of with 1 blocker  " gainAdvantage oneBlocker
  report' "Gained advantage of with 2 blockers " gainAdvantage twoBlockers

report :: [[Attack]] -> String -> Condition -> Condition -> IO ()
report attacks msg a b = putStrLn $ msg ++ " : " ++ intercalate "  " [ printf "%3.0f%% (%3d)" (percentage attacks a b) (sampleSize attacks b) | attacks <- attacks ]

-- Conditions.
winPoint        (_, r) = case r of { WinPoint  -> True; _ -> False }
lostPoint       (_, r) = case r of { LostPoint -> True; _ -> False }
gainAdvantage   (_, r) = case r of { WinPoint -> True; DefenseAttack -> True; OffenseFreeBall -> True; _ -> False }
blockTouched    (b, _) = case b of { BlockTouch _ -> True; _ -> False }
blockMissed     (b, _) = case b of { BlockMiss  _ -> True; _ -> False }
blockAttempt    (b, _) = case b of { BlockTouch _ -> True; BlockMiss _ -> True; NoBlock -> False }
--allAttacks             = const True
--lostAdvantage          = not . gainAdvantage
--noBlockOrMissed        = not . blockTouched
--noBlock                = not . blockAttempt

noBlockers  (b, _) = case b of { NoBlock -> True; _ -> False }
oneBlocker  (b, _) = case b of { BlockTouch 1 -> True; _ -> False }
twoBlockers (b, _) = case b of { BlockTouch 2 -> True; _ -> False }

percentage :: [Attack] -> Condition -> Condition -> Double
percentage attacks a b = 100 * fromIntegral (length a') / fromIntegral (length b')
  where
  b' = filter b attacks
  a' = filter a b'

sampleSize :: [Attack] -> Condition -> Int
sampleSize attacks a = length $ filter a attacks

parseMatch :: String -> [Attack]
parseMatch = map parseAttack . words . unlines . map (takeWhile (/= '#')) . lines

parseAttack :: String -> Attack
parseAttack a = case a of
  n : 't' : r | elem n "123" -> (BlockTouch $ read [n], parseResult r)
  n : 'm' : r | elem n "123" -> (BlockMiss  $ read [n], parseResult r)
  r                          -> (NoBlock,             parseResult r)

parseResult :: String -> Result
parseResult a = case a of
  "w"  -> WinPoint
  "l"  -> LostPoint
  "da" -> DefenseAttack
  "df" -> DefenseFreeBall
  "oa" -> OffenseAttack
  "of" -> OffenseFreeBall
  _ -> error $ "Invalid result: " ++ a

