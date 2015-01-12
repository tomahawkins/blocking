{
module Match
  ( Match
  , Set    (..)
  , Volley (..)
  , Attack (..)
  , Team   (..)
  , parseMatch
  ) where

type Match   = [Set]                                     -- A match is a list of sets.
type Set     = [Volley]                                  -- A set is a sequence of volleys.
data Volley' = Volley' Team [Attack]      deriving Show  -- A volley is a side that serves and a sequence of attacks.
data Volley  = Volley  Team [Attack] Team deriving Show  -- A volley is a side that serves, a sequence of attacks, and who wins.
data Attack  = Attack  Team Int           deriving Show  -- Attacking side an number of opposing blockers.
data Team    = A | B                      deriving (Show, Eq)  -- Team A or side B.

parseMatch :: String -> Match
parseMatch = swapTeams . match . concat . words . unlines . map (takeWhile (/= '#')) . lines

swapTeams :: Match -> Match
swapTeams a = case a of
  a : b : c -> a : set b : swapTeams c
  a -> a
  where
  set :: Set -> Set
  set = map volley

  volley :: Volley -> Volley
  volley (Volley a b c) = Volley (team a) (map attack b) (team c)

  attack :: Attack -> Attack
  attack (Attack a b) = Attack (team a) b

  team :: Team -> Team
  team A = B
  team B = A

parseError :: String -> a
parseError a = case a of
  [] -> error "Parse error: no tokens left to parse."
  a  -> error $ "Parse error: unexpected token sequence:  " ++ a

volleys' :: [Volley'] -> Team -> [Volley]
volleys' a winner = f a
  where
  f :: [Volley'] -> [Volley]
  f a = case a of
    [] -> []
    Volley' a b : []                     -> Volley a b winner : []
    Volley' a b : d@(Volley' c _) : rest -> Volley a b c : f (d : rest)

}

%name match
%tokentype { Char }
%error { parseError }

%expect 0

%token

"."           { '.' }
";"           { ';' }
"a"           { 'a' }
"b"           { 'b' }
"j"           { 'j' }
"k"           { 'k' }
"s"           { 's' }
"w"           { 'w' }
"0"           { '0' }
"1"           { '1' }
"2"           { '2' }
"3"           { '3' }

%%

Match :: { Match }
:           { [] }
| Match Set { $1 ++ [$2] }

Set :: { Set }
: Volleys Win { volleys' $1 $2 }

Win :: { Team }
: Team "w" "." { $1 }

Team :: { Team }
: "a" { A }
| "j" { A }
| "b" { B }
| "k" { B }

Volleys :: { [Volley'] }
:                { [] }
| Volleys Volley { $1 ++ [$2] }

Volley :: { Volley' }
: Team "s" Attacks "." { Volley' $1 $3 }

Attacks :: { [Attack] }
:                { [] }
| Attacks Attack { $1 ++ [$2] }

Attack :: { Attack }
: Team     Blockers { Attack $1 $2 }
| Team ";" Blockers { Attack $1 $3 }  -- Attack on 2.

Blockers :: { Int }
: "0" { 0 }
| "1" { 1 }
| "2" { 2 }
| "3" { 3 }

