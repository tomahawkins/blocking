{
module Match
  ( Match
  , Set    (..)
  , Volley (..)
  , Attack (..)
  , Team   (..)
  , parseMatch
  ) where

type Match  = [Set]                                -- A match is a list of sets.
data Set    = Set [Volley] Team     deriving Show  -- A set is a sequence of volleys and who wins.
data Volley = Volley Team [Attack]  deriving Show  -- A volley is a side that serves and a sequence of attacks.
data Attack = Attack Team Int       deriving Show  -- Attacking side an number of opposing blockers.
data Team   = A | B                 deriving (Show, Eq)  -- Team A or side B.

parseMatch :: String -> Match
parseMatch = match . concat . words . unlines . map (takeWhile (/= '#')) . lines

}

%name match
%tokentype { Char }
%error { parseError }

%expect 0

%token

"."           { '.' }
"a"           { 'a' }
"b"           { 'b' }
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
: Volleys Win { Set $1 $2 }

Win :: { Team }
: Team "w" "." { $1 }

Team :: { Team }
: "a" { A }
| "b" { B }

Volleys :: { [Volley] }
:                { [] }
| Volleys Volley { $1 ++ [$2] }

Volley :: { Volley }
: Team "s" Attacks "." { Volley $1 $3 }

Attacks :: { [Attack] }
:                { [] }
| Attacks Attack { $1 ++ [$2] }

Attack :: { Attack }
: Team Blockers { Attack $1 $2 }

Blockers :: { Int }
: "0" { 0 }
| "1" { 1 }
| "2" { 2 }
| "3" { 3 }

{
parseError :: String -> a
parseError a = case a of
  [] -> error "Parse error: no tokens left to parse."
  a  -> error $ "Parse error: unexpected token sequence:  " ++ a
}

