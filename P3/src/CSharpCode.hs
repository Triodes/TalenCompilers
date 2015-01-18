module CSharpCode where

import Prelude hiding (LT, GT, EQ)
import Data.Map as M hiding (map)
import Data.Char
import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM


data ValueOrAddress = Value | Address
    deriving Show

type SSMExpr = (ValueOrAddress -> ParamEnv -> Code)
type SSMStat = (ParamEnv -> (Code, [String]))

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

test = "class Hello { void main() { 5 + 5; } }"

fMembMeth :: Type -> Token -> [Decl] -> SSMStat -> Code
fMembMeth t (LowerId x) ps s = [LABEL x,LINK 0] ++ (fst stats) ++ [UNLINK] ++ [RET]
    where
        stats = s env
        envP  = fromList $ zip [x | (Decl _ (LowerId x)) <- ps] [(-(length ps) - 1)..]
        envD  = fromList $ zip (snd stats) [1..]
        env   = union envD envP
       -- makeEnv ps@((Decl t (LowerId n)):xs) i = M.insert n (i - length ps) (makeEnv xs (i + 1))
        --makeEnv []                   i = M.empty

fStatDecl :: Decl -> SSMStat
fStatDecl (Decl _ (LowerId x)) _env = ([], [x])

fStatExpr :: SSMExpr -> SSMStat
fStatExpr e env = (e Value env ++ [pop], [])

fStatIf :: SSMExpr -> SSMStat -> SSMStat -> SSMStat
fStatIf e s1 s2 env = (c ++ [BRF (n1 + 2)] ++ (fst $ s1 env) ++ [BRA n2] ++ (fst $ s2 env), [])
    where
        c        = e Value env
        (n1, n2) = (codeSize (fst $ s1 env), codeSize (fst $ s2 env))

fStatWhile :: SSMExpr -> SSMStat -> SSMStat
fStatWhile e s1 env = ([BRA n] ++ (fst $ s1 env) ++ c ++ [BRT (-(n + k + 2))], [])
    where
        c = e Value env
        (n, k) = (codeSize (fst $ s1 env), codeSize c)

fStatReturn :: SSMExpr -> SSMStat
fStatReturn e env = (e Value env ++ [STR R3] ++ [UNLINK] ++ [RET], [])

fStatBlock :: [SSMStat] -> SSMStat
fStatBlock ss env = (concat $ fst stats, concat $ snd stats)
    where stats = unzip $ map ($ env) ss

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

-- as methods are expressions they must leave they're result on the stack. for methods that actually return somesthing
-- this a value is stored in the RR. For methods that don't return anything we put whatever is in RR on the stack.
-- This doesn't matter because when the result of an expression isn't needed anymore it is discarded with an AJS -1.

fExprCall :: Token -> [SSMExpr] -> SSMExpr
fExprCall (LowerId "print") ps va env = concatMap (\p -> p Value env) ps ++ replicate (length ps) (TRAP 0)  ++ [LDR R3]
fExprCall (LowerId m) ps va env       = concatMap (\p -> p Value env) ps ++ [Bsr m] ++ [AJS (-(length ps))] ++ [LDR R3]

opCodes :: Map String Instr
opCodes = fromList [ ("+", ADD), ("-", SUB),  ("*", MUL), ("/", DIV), ("%", MOD)
                   , ("<=", LE), (">=", GE),  ("<", LT),  (">", GT),  ("==", EQ)
                   , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                   ]

