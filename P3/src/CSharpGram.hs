module CSharpGram where

import ParseLib.Abstract hiding (braced, bracketed, parenthesised)
import CSharpLex
import Data.List


data Class = Class Token [Member]
    deriving Show

data Member = MemberD Decl
            | MemberM Type Token [Decl] Stat
            deriving Show

data Stat = StatDecl   Decl
          | StatExpr   Expr
          | StatIf     Expr Stat Stat
          | StatWhile  Expr Stat
          | StatReturn Expr
          | StatBlock  [Stat]
          deriving Show

data Expr = ExprConst  Token
          | ExprVar    Token
          | ExprOper   Token Expr Expr
          | ExprCall   Token [Expr]
          deriving Show

data Decl = Decl Type Token
    deriving Show

data Type = TypeVoid
          | TypePrim  Token
          | TypeObj   Token
          | TypeArray Type
          deriving (Eq,Show)


parenthesised p = pack (symbol POpen) p (symbol PClose)
bracketed     p = pack (symbol SOpen) p (symbol SClose)
braced        p = pack (symbol COpen) p (symbol CClose)

pExprSimple :: Parser Token Expr
pExprSimple =  ExprConst <$> sConst
           <|> ExprVar   <$> sLowerId
           <|> pExprCall
           <|> parenthesised pExpr

-- Takes 'layers' of operators (sorted by priority) and builds a left associative expression tree
pExpr :: Parser Token Expr
pExpr = foldr opLayer pExprSimple operatorsPrio

opLayer :: [Token] -> Parser Token Expr -> Parser Token Expr
opLayer ops p = chainl p (choice (map f ops))
    where
        f op = ExprOper <$> symbol op

{-
   Left associative operators, except for assignment
   Assignment has lowest priority, so chainl still yields valid expression trees
       
   Sorted by operator priority, low to high (for use with foldr)
-}
operatorsPrio = [
        [Operator "="],
        [Operator "||"],
        [Operator "&&"],
        [Operator "^"],
        [Operator "!=", Operator "=="],
        [Operator "<", Operator "<=", Operator ">", Operator ">="],
        [Operator "+", Operator "-"],
        [Operator "*", Operator "/", Operator "%"]
    ]

-- Parses method calls as expression (including any expressions as method arguments)
pExprCall :: Parser Token Expr
pExprCall = ExprCall <$> sLowerId <*> parenthesised (option (listOf pExpr (symbol Comma)) [])

pMember :: Parser Token Member
pMember =  MemberD <$> pDeclSemi
       <|> pMeth

pStatDecl :: Parser Token Stat
pStatDecl =  pStat
         <|> StatDecl <$> pDeclSemi

pStat :: Parser Token Stat
pStat =  StatExpr <$> pExpr <*  sSemi
     <|> StatIf     <$ symbol KeyIf     <*> parenthesised pExpr <*> pStat <*> optionalElse
     <|> StatWhile  <$ symbol KeyWhile  <*> parenthesised pExpr <*> pStat
     <|> StatReturn <$ symbol KeyReturn <*> pExpr               <*  sSemi
     <|> pBlock
     where optionalElse = option ((\_ x -> x) <$> symbol KeyElse <*> pStat) (StatBlock [])


pBlock :: Parser Token Stat
pBlock = StatBlock <$> braced (many pStatDecl)

sortStats x = sortBy f x
    where f (StatDecl _) (StatDecl _) = EQ
          f (StatDecl _) _            = LT
          f _            (StatDecl _) = GT
          f _            _            = EQ

t1 = "()"
t2 = "(int a)"
t3 = "test(int a, int b);"

pMeth :: Parser Token Member
pMeth = MemberM <$> methRetType <*> sLowerId <*> methArgList <*> pBlock
    where
        methRetType = pType <|> (const TypeVoid <$> symbol KeyVoid)
        methArgList = parenthesised (option (listOf pDecl (symbol Comma)) [])

pType0 :: Parser Token Type
pType0 =  TypePrim <$> sStdType
      <|> TypeObj  <$> sUpperId

pType :: Parser Token Type
pType = foldr (const TypeArray) <$> pType0 <*> many (bracketed (succeed ()))


pDecl :: Parser Token Decl
pDecl = Decl <$> pType <*> sLowerId

pDeclSemi :: Parser Token Decl
pDeclSemi = const <$> pDecl <*> sSemi

pClass :: Parser Token Class
pClass = Class <$ symbol KeyClass <*> sUpperId <*> braced (many pMember)

