module CSharpCode where

import Prelude hiding (LT, GT, EQ)
import Data.Map as M hiding (map)
import Data.List hiding (union)
import Data.Char
import Control.Arrow
import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM


data ValueOrAddress = Value | Address
    deriving Show

-- List of declared variables
type Vars = [String]

-- Return type for expressions
type SSMExpr = (ValueOrAddress -> FinalEnv -> Code)

-- Return type for statements
type SSMStat = (FinalEnv -> (Code, Vars))

-- Return type for members
type SSMMemb = (GlobalEnv -> (Code, Vars))

-- Local, Global and combined environments. Tupled so local and global vars with the same name can coëxist.
-- Local variables get priority over global ones in fExprVars
type LocalEnv  = Map String Int
type GlobalEnv = Map String Int
type FinalEnv  = (LocalEnv, GlobalEnv)

-- Algebra with modified return types.
codeAlgebra :: CSharpAlgebra Code SSMMemb SSMStat SSMExpr
codeAlgebra =
    ( fClas
    , (fMembDecl, fMembMeth)
    , (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatReturn, fStatBlock)
    , (fExprCon, fExprVar, fExprOp, fExprCall)
    )

-- AJS 1: dummy space for undeclared vars. Using undeclared vars will point to this space
-- LDRR R4: create a pointer to the global var space.
-- AJS (size env): reserve space for global vars.
fClas :: Token -> [SSMMemb] -> Code
fClas c ms = [AJS 1, LDRR R4 SP, AJS (size env), Bsr "main", HALT] ++ concat (fst membs)
    where membs = unzip $ map ($ env) ms -- Unzip the tuples returned by ms
          env   = fromList $ zip (nub $ concat $ snd membs) [1..] -- turn the lists of vars in an environment

-- returns no code and the name of the declared var
fMembDecl :: Decl -> SSMMemb
fMembDecl (Decl _ (LowerId x)) _env = ([], [x])

-- Constructs method entrance and exit, reserves space for local vars with LINK (size envD)
fMembMeth :: Type -> Token -> [Decl] -> SSMStat -> SSMMemb
fMembMeth t (LowerId x) ps s gEnv = ([LABEL x,LINK (size envD)] ++ fst stats ++ [UNLINK] ++ [RET], [])
    where
        stats = s env
        envP  = fromList $ zip [x | (Decl _ (LowerId x)) <- ps] [(-(length ps) - 1)..] -- construct env with params
        envD  = fromList $ zip (nub $ snd stats) [1..] -- construct env with local vars
        env   = (union envD envP, gEnv) -- combine param and local var environments and tuple with global environment

-- returns no code and the name of the declared var
fStatDecl :: Decl -> SSMStat
fStatDecl (Decl _ (LowerId x)) _env = ([], [x])

-- evaluates an expression on a single line and "garbage collects" the returned value when no longer needed.
fStatExpr :: SSMExpr -> SSMStat
fStatExpr e env = (e Value env ++ [pop], [])

-- evaluates an if statement
fStatIf :: SSMExpr -> SSMStat -> SSMStat -> SSMStat
fStatIf e s1 s2 env = (c ++ [BRF (n1 + 2)] ++ fst stat1 ++ [BRA n2] ++ fst stat2, vars)
    where -- calculates the size of the then and else blocks. Get the vars given by decls in those blocks.
        stat1    = s1 env
        stat2    = s2 env
        vars     = snd stat1 ++ snd stat2
        c        = e Value env
        (n1, n2) = (codeSize (fst stat1), codeSize (fst stat2))

-- evaluates a while statement
fStatWhile :: SSMExpr -> SSMStat -> SSMStat
fStatWhile e s1 env = ([BRA n] ++ fst stat ++ c ++ [BRT (-(n + k + 2))], snd stat)
    where -- calculates the size of the while block. Get the vars given by decls in this block.
        stat   = s1 env 
        c      = e Value env
        (n, k) = (codeSize (fst $ s1 env), codeSize c)

-- evaluates the expression in a return statement, stores its value in the RR and returns
fStatReturn :: SSMExpr -> SSMStat
fStatReturn e env = (e Value env ++ [STR R3] ++ [UNLINK] ++ [RET], [])

-- evaluates a block of statements
fStatBlock :: [SSMStat] -> SSMStat
fStatBlock ss env = (concat *** concat) stats
    where stats = unzip $ map ($ env) ss

-- evaluates constants
fExprCon :: Token -> SSMExpr
fExprCon c va _env = case c of
                    ConstInt  n -> [LDC n]
                    ConstBool b -> [LDC (if b then -1 else 0)]
                    ConstChar c -> [LDC (ord c)]

-- evaluates the address or value of a variable. Local vars get priority over global ones.
fExprVar :: Token -> SSMExpr
fExprVar (LowerId x) va env = case va of
                                  Value    ->  if local then [LDL offset]  else [LDR R4, LDA offset]
                                  Address  ->  if local then [LDLA offset] else [LDR R4, LDAA offset]
    where lEnv   = fst env
          gEnv   = snd env
          local  = member x lEnv
          global = member x gEnv
          offset 
            | local     = lEnv ! x 
            | global    = gEnv ! x 
            | otherwise = 0       -- zero points to the dummy space created by fClass

-- evaluates operators
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

