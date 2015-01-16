module CSharpCode where

import Prelude hiding (LT, GT, EQ)
import Data.Map as M
import Data.Char
import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM


data ValueOrAddress = Value | Address
    deriving Show

type SSMExpr = (ValueOrAddress -> ParamEnv -> Code)
type SSMStat = (ParamEnv -> Code)

type ParamEnv = Map String Int

codeAlgebra :: CSharpAlgebra Code Code SSMStat SSMExpr
codeAlgebra =
    ( fClas
    , (fMembDecl, fMembMeth)
    , (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatReturn, fStatBlock)
    , (fExprCon, fExprVar, fExprOp, fExprCall)
    )

fClas :: Token -> [Code] -> Code
fClas c ms = [Bsr "main", HALT] ++ concat ms

fMembDecl :: Decl -> Code
fMembDecl d = []

test = "class Hello { void main() { test() + 5; } }"

fMembMeth :: Type -> Token -> [Decl] -> SSMStat -> Code
fMembMeth t (LowerId x) ps s = [LABEL x,LINK 0] ++ s env ++ [UNLINK] ++ [RET]
    where
        env = fromList $ zip [x | (Decl _ (LowerId x)) <- ps] [(-(length ps) - 1)..]
       -- makeEnv ps@((Decl t (LowerId n)):xs) i = M.insert n (i - length ps) (makeEnv xs (i + 1))
        --makeEnv []                   i = M.empty

fStatDecl :: Decl -> SSMStat
fStatDecl _d _env = []

fStatExpr :: SSMExpr -> SSMStat
fStatExpr e env = e Value env ++ [pop]

fStatIf :: SSMExpr -> SSMStat -> SSMStat -> SSMStat
fStatIf e s1 s2 env = c ++ [BRF (n1 + 2)] ++ (s1 env) ++ [BRA n2] ++ (s2 env)
    where
        c        = e Value env
        (n1, n2) = (codeSize (s1 env), codeSize (s2 env))

fStatWhile :: SSMExpr -> SSMStat -> SSMStat
fStatWhile e s1 env = [BRA n] ++ (s1 env) ++ c ++ [BRT (-(n + k + 2))]
    where
        c = e Value env
        (n, k) = (codeSize (s1 env), codeSize c)

fStatReturn :: SSMExpr -> SSMStat
fStatReturn e env = e Value env ++ [STR R3] ++ [UNLINK] ++ [RET]

fStatBlock :: [SSMStat] -> SSMStat
fStatBlock ss env = concatMap ($ env) ss

fExprCon :: Token -> SSMExpr
fExprCon c va _env = case c of
                    ConstInt  n -> [LDC n]
                    ConstBool b -> [LDC (if b then 1 else 0)]
                    ConstChar c -> [LDC (ord c)]

fExprVar :: Token -> SSMExpr
fExprVar (LowerId x) va env = case va of
                                  Value    ->  [LDL  loc]
                                  Address  ->  [LDLA loc]
    where loc = if member x env then env ! x else 37

fExprOp :: Token -> SSMExpr -> SSMExpr -> SSMExpr
fExprOp (Operator "=") e1 e2 va env = e2 Value env ++ [LDS 0] ++ e1 Address env ++ [STA 0]
fExprOp (Operator op)  e1 e2 va env = e1 Value env ++ e2 Value env ++ [opCodes ! op]

fExprCall :: Token -> [SSMExpr] -> SSMExpr
fExprCall (LowerId m) ps va env = concatMap (\p -> p Value env) ps ++ [Bsr m] ++ [AJS (-(length ps))] ++ [LDR R3]


opCodes :: Map String Instr
opCodes = fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                   , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                   , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                   ]

