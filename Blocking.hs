module Main (main) where

import Data.List
import System.Environment
import Text.Printf

data Block
  = BlockTouch
  | BlockMiss
  | NoBlock
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
  attacks <- mapM readFile args >>= return . map (map parseAttack . lines)
  let report' = report attacks
  report' "Blocks touched   of all blocks    " blockTouched  blockAttempt
  putStrLn ""
  report' "Win point        of all blocks    " winPoint      blockAttempt
  report' "Lost point       of all blocks    " lostPoint     blockAttempt
  putStrLn ""
  report' "Win point        of blocks touched" winPoint      blockTouched
  report' "Lost point       of blocks touched" lostPoint     blockTouched
  putStrLn ""
  report' "Win point        of blocks missed " winPoint      blockMissed
  report' "Lost point       of blocks missed " lostPoint     blockMissed
  putStrLn ""
  report' "Gained advantage of all blocks    " gainAdvantage blockAttempt
  report' "Gained advantage of blocks touched" gainAdvantage blockTouched
  report' "Gained advantage of blocks missed " gainAdvantage blockMissed

report :: [[Attack]] -> String -> Condition -> Condition -> IO ()
report attacks msg a b = putStrLn $ msg ++ " : " ++ intercalate "  " [ printf "%2.0f%%" $ percentage attacks a b | attacks <- attacks ]

-- Conditions.
winPoint        (_, r) = case r of { WinPoint  -> True; _ -> False }
lostPoint       (_, r) = case r of { LostPoint -> True; _ -> False }
gainAdvantage   (_, r) = case r of { WinPoint -> True; DefenseAttack -> True; OffenseFreeBall -> True; _ -> False }
blockTouched    (b, _) = case b of { BlockTouch -> True; _ -> False }
blockMissed     (b, _) = case b of { BlockMiss  -> True; _ -> False }
blockAttempt    (b, _) = case b of { BlockTouch -> True; BlockMiss -> True; NoBlock -> False }
--allAttacks             = const True
--lostAdvantage          = not . gainAdvantage
--noBlockOrMissed        = not . blockTouched
--noBlock                = not . blockAttempt

percentage :: [Attack] -> Condition -> Condition -> Double
percentage attacks a b = 100 * fromIntegral (length a') / fromIntegral (length b')
  where
  b' = filter b attacks
  a' = filter a b'

parseAttack :: String -> Attack
parseAttack a = case a of
  't' : a -> (BlockTouch, parseResult a)
  'm' : a -> (BlockMiss,  parseResult a)
  'n' : a -> (NoBlock,    parseResult a)
  a -> error $ "Invalid block: " ++ a

parseResult :: String -> Result
parseResult a = case a of
  "w"  -> WinPoint
  "l"  -> LostPoint
  "da" -> DefenseAttack
  "df" -> DefenseFreeBall
  "oa" -> OffenseAttack
  "of" -> OffenseFreeBall
  _ -> error $ "Invalid result: " ++ a

