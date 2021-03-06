module CSharpLex where

import Data.Char
import Control.Monad
import ParseLib.Abstract


data Token = POpen    | PClose      -- parentheses     ()
           | SOpen    | SClose      -- square brackets []
           | COpen    | CClose      -- curly braces    {}
           | Comma    | Semicolon
           | KeyIf    | KeyElse
           | KeyWhile | KeyReturn
           | KeyTry   | KeyCatch
           | KeyClass | KeyVoid
           | StdType   String       -- the 8 standard types
           | Operator  String       -- the 15 operators
           | UpperId   String       -- uppercase identifiers
           | LowerId   String       -- lowercase identifiers
           | ConstInt  Int
           | ConstBool Bool
           | ConstChar Char
           deriving (Eq, Show)

keyword :: String -> Parser Char String
keyword [] = succeed ""
keyword xs@(x:_) | isLetter x = do ys <- greedy (satisfy isAlphaNum)
                                   guard (xs == ys)
                                   return ys
                 | otherwise  = token xs


greedyChoice :: [Parser s a] -> Parser s a
greedyChoice = foldr (<<|>) empty


terminals :: [(Token, String)]
terminals =
    [ ( POpen     , "("      )
    , ( PClose    , ")"      )
    , ( SOpen     , "["      )
    , ( SClose    , "]"      )
    , ( COpen     , "{"      )
    , ( CClose    , "}"      )
    , ( Comma     , ","      )
    , ( Semicolon , ";"      )
    , ( KeyIf     , "if"     )
    , ( KeyElse   , "else"   )
    , ( KeyWhile  , "while"  )
    , ( KeyReturn , "return" )
    , ( KeyTry    , "try"    )
    , ( KeyCatch  , "catch"  )
    , ( KeyClass  , "class"  )
    , ( KeyVoid   , "void"   )
    ]


lexIgnore :: Parser Char String
lexIgnore = concat <$> greedy (lexWhiteSpace <|> lexComments)

lexWhiteSpace :: Parser Char String
lexWhiteSpace = f <$> satisfy isSpace
    where f x = [x]

-- Parses the two types of comments:
-- Single line ('// ...') and multiline ('/* ... */')
lexComments :: Parser Char String
lexComments = token "//" *> many anyNoEol <* eol
    <|> token "/*" *> many anySymbol <* token "*/"

anyNoEol :: Parser Char Char
anyNoEol = satisfy (\x -> x /= '\r' && x /= '\n')

eol :: Parser Char String
eol = token "\n" <|> token "\r\n"

lexLowerId :: Parser Char Token
lexLowerId = (\x xs -> LowerId (x:xs)) <$> satisfy isLower <*> greedy (satisfy isAlphaNum)

lexUpperId :: Parser Char Token
lexUpperId = (\x xs -> UpperId (x:xs)) <$> satisfy isUpper <*> greedy (satisfy isAlphaNum)

lexConstInt :: Parser Char Token
lexConstInt = (ConstInt . read) <$> greedy1 (satisfy isDigit)

-- Can also use the read function, but Haskell uses capitalized booleans
lexConstBool :: Parser Char Token
lexConstBool = (ConstBool . read . \(x:xs) -> toUpper x : xs) <$> (token "true" <|> token "false")

-- Take any single symbol between single quotes
lexConstChar :: Parser Char Token
lexConstChar = ConstChar <$> pack (symbol '\'') anySymbol (symbol '\'')

lexEnum :: (String -> Token) -> [String] -> Parser Char Token
lexEnum f xs = f <$> choice (map keyword xs)

lexTerminal :: Parser Char Token
lexTerminal = choice [t <$ keyword s | (t,s) <- terminals]


stdTypes :: [String]
stdTypes = ["int", "long", "double", "float", "byte", "short", "bool", "char"]

operators :: [String]
operators = ["+=","-=","*=","/=","%=","^=","+", "-", "*", "/", "%", "&&", "||", "^", "<=", "<", ">=", ">", "==", "!=", "="]


lexToken :: Parser Char Token
lexToken = greedyChoice
             [ lexTerminal
             , lexEnum StdType stdTypes
             , lexEnum Operator operators
             , lexConstInt
             , lexConstBool
             , lexConstChar
             , lexLowerId
             , lexUpperId
             ]

-- Now uses lexIgnore to also discard comments
lexicalScanner :: Parser Char [Token]
lexicalScanner = lexIgnore *> greedy (lexToken <* lexIgnore) <* eof


sStdType :: Parser Token Token
sStdType = satisfy isStdType
    where isStdType (StdType _) = True
          isStdType _           = False

sUpperId :: Parser Token Token
sUpperId = satisfy isUpperId
    where isUpperId (UpperId _) = True
          isUpperId _           = False

sLowerId :: Parser Token Token
sLowerId = satisfy isLowerId
    where isLowerId (LowerId _) = True
          isLowerId _           = False

sConst :: Parser Token Token
sConst  = satisfy isConst
    where isConst (ConstInt  _) = True
          isConst (ConstBool _) = True
          isConst (ConstChar _) = True
          isConst _             = False

sOperator :: Parser Token Token
sOperator = satisfy isOperator
    where isOperator (Operator _) = True
          isOperator _            = False


sSemi :: Parser Token Token
sSemi =  symbol Semicolon

