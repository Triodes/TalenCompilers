{
module Main where
}
%name parseArrow
%tokentype { Token }
%error { parseError }

%token
    ->       { Arrow }
    '.'      { Dot }
    ','      { Comma }
    go       { Go }
    take     { Take }
    mark     { Mark }
    nothing  { NothingT }
    turn     { Turn }
    case     { Case }
    of       { Of }
    end      { End }
    left     { LeftT }
    right    { RightT }
    front    { Front }
    ';'      { Semicolon }
    Empty    { Empty }
    Lambda   { Lambda }
    Debris   { Debris }
    Asteroid { Asteroid }
    Boundary { Boundary }
    '_'      { Underscore }
    Ident    { Ident }

