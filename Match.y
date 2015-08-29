{
module Match
  ( Match          (..)
  , Set            (..)
  , Volley         (..)
  , Attack         (..)
  , AttackLocation (..)
  , Team           (..)
  , parseMatch
  ) where
}

%name sets
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
"m"           { 'm' }
"0"           { '0' }
"1"           { '1' }
"2"           { '2' }
"3"           { '3' }

%%

Sets :: { [Set] }
:          { [] }
| Sets Set { $1 ++ [$2] }

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
: Team AttackOnTwo AttackLocation Blockers { Attack $1 $2 $3 $4 }

AttackOnTwo :: { Bool }
:      { False }
| ";"  { True  }

AttackLocation :: { Maybe AttackLocation }
AttackLocation
  :      { Nothing }
  | "s"  { Just StrongSide }
  | "w"  { Just WeakSide   }
  | "m"  { Just Middle     }
  | "b"  { Just Backrow    }

Blockers :: { Int }
: "0" { 0 }
| "1" { 1 }
| "2" { 2 }
| "3" { 3 }

{

data Match   = Match String String [Set]                 -- A match is the team names and a list of sets.
type Set     = [Volley]                                  -- A set is a sequence of volleys.
data Volley' = Volley' Team [Attack]      deriving Show  -- A volley is a side that serves and a sequence of attacks.
data Volley  = Volley  Team [Attack] Team deriving Show  -- A volley is a side that serves, a sequence of attacks, and who wins.
data Attack  = Attack  Team Bool (Maybe AttackLocation) Int deriving Show  -- Attacking side, attack-on-two, location, number of blockers.
data AttackLocation = StrongSide | WeakSide | Middle | Backrow deriving Show
data Team    = A | B                      deriving (Show, Eq)  -- Team A or side B.

parseMatch :: String -> Match
parseMatch a = Match teamA teamB $ swapTeams $ sets $ concat game
  where
  teamA : "vs" : teamB : game = words . unlines . map (takeWhile (/= '#')) $ lines a

swapTeams :: [Set] -> [Set]
swapTeams a = case a of
  a : b : c -> a : set b : swapTeams c
  a -> a
  where
  set :: Set -> Set
  set = map volley

  volley :: Volley -> Volley
  volley (Volley a b c) = Volley (team a) (map attack b) (team c)

  attack :: Attack -> Attack
  attack (Attack a b c d) = Attack (team a) b c d

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
