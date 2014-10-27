module Main (main) where

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

main :: IO ()
main = do
  args <- getArgs
  attacks <- mapM readFile args >>= return . map parseAttack . lines . concat
  let percent msg a b = printf (msg ++ " : %2.0f%%\n") $ percentage attacks a b
  printf  "Total recorded attacks                        : %d\n"      $ length attacks
  percent "Block attempts   of total attacks            " blockAttempt  allAttacks
  percent "Block touches    of block attempts           " blockTouched  blockAttempt
  percent "Block misses     of block attempts           " blockMissed   blockAttempt
  putStrLn ""
  percent "Win point        of block attempts           " winPoint      blockAttempt
  percent "Lost point       of block attempts           " lostPoint     blockAttempt
  putStrLn ""
  percent "Win point        of block touched            " winPoint      blockTouched
  percent "Lost point       of block touched            " lostPoint     blockTouched
  putStrLn ""
  percent "Win point        of block missed             " winPoint      blockMissed
  percent "Lost point       of block missed             " lostPoint     blockMissed
  putStrLn ""
  percent "Win point        of block missed or no block " winPoint      noBlockOrMissed
  percent "Lost point       of block missed or no block " lostPoint     noBlockOrMissed
  putStrLn ""
  percent "Win point        of no block                 " winPoint      noBlock
  percent "Lost point       of no block                 " lostPoint     noBlock
  putStrLn ""
  percent "Gained advantage of block attempts           " gainAdvantage blockAttempt
  percent "Gained advantage of block touched            " gainAdvantage blockTouched
  percent "Gained advantage of block missed             " gainAdvantage blockMissed
  percent "Gained advantage of block missed or no block " gainAdvantage noBlockOrMissed
  percent "Gained advantage of no block                 " gainAdvantage noBlock

allAttacks             = const True
winPoint        (_, r) = case r of { WinPoint  -> True; _ -> False }
lostPoint       (_, r) = case r of { LostPoint -> True; _ -> False }
gainAdvantage   (_, r) = case r of { WinPoint -> True; DefenseAttack -> True; OffenseFreeBall -> True; _ -> False }
--lostAdvantage          = not . gainAdvantage
blockTouched    (b, _) = case b of { BlockTouch -> True; _ -> False }
noBlockOrMissed        = not . blockTouched
blockMissed     (b, _) = case b of { BlockMiss  -> True; _ -> False }
blockAttempt    (b, _) = case b of { BlockTouch -> True; BlockMiss -> True; NoBlock -> False }
noBlock                = not . blockAttempt

percentage :: [Attack] -> (Attack -> Bool) -> (Attack -> Bool) -> Double
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

