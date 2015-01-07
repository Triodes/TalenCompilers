{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$pm    = [\+\-]

tokens :-

$white+     ;
"--".*      ;
\->         {const Arrow }
\.          {const Dot }
\,          {const Comma }
\;          {const Semicolon }
go          {const Go }
take        {const Take }
mark        {const Mark }
nothing     {const NothingT }
turn        {const Turn }
case        {const Case }
of          {const Of }
end         {const End }
left        {const LeftT }
right       {const RightT }
front       {const Front }
Empty       {const Empty }
Lambda      {const Lambda }
Debris      {const Debris }
Asteroid    {const Asteroid }
Boundary    {const Boudary }
\_          {const Underscore }
[$alpha $digit $pm]+ {\s -> Ident s}



{
-- Each action has type :: String -> Token

-- The token type:
data Token = Dot | Comma | Go | Take | Mark | Semicolon
           | NothingT | Turn | Case | Of | End
           | LeftT | RightT | Front | Empty | Arrow
           | Lambda | Debris | Asteroid | Boudary | Underscore
           | Ident String
           deriving (Show)

main = do
  s <- getContents
  print (alexScanTokens s)
}