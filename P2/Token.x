{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$pm    = [\+\-]

tokens :-

\->         { Arrow }
\.          { Dot }
\,          { Comma }
\;          { Semicolon }
go          { Go }
take        { Take }
mark        { Mark }
nothing     { NothingT }
turn        { Turn }
case        { Case }
of          { Of }
end         { End }
left        { LeftT }
right       { RightT }
front       { Front }
Empty       { Empty }
Lambda      { Lambda }
Debris      { Debris }
Asteroid    { Asteroid }
Boundary    { Boudary }
\_          { Underscore }
[$alpha $digit $pm]+ { Ident }



{
-- Each action has type :: String -> Token

-- The token type:
data Token = Dot | Comma | Go | Take | Mark | Semicolon
           | NothingT | Turn | Case | Of | End
           | LeftT | RightT | Front | Empty | Arrow
           | Lambda | Debris | Asteroid | Boudary | Underscore
           | Ident String

main = do
  s <- getContents
  print (alexScanTokens s)
}