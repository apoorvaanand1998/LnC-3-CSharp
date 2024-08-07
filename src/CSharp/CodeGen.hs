module CSharp.CodeGen where

import CSharp.AbstractSyntax
import CSharp.Algebra

import SSM

import Prelude hiding (LT, GT, EQ)
import qualified Data.Map as M
import qualified Data.Bifunctor as B
import Data.List ( singleton )
import Debug.Trace

{-
  This file contains a starting point for the code generation.
-}

-- The types that we generate for each datatype: our type variables for the algebra.
-- Change these definitions instead of the function signatures to get better type errors.
type C = Env -> (Code, Env)     -- Class
type M = Env -> (Code, Env)     -- Member
type S = Env -> (Code, Env)     --  Statement
type E = ValueOrAddress -> Env -> (Code, Env) -- Expression

type Env = M.Map Ident (RetType, Int)


codeAlgebra :: CSharpAlgebra C M S E
codeAlgebra = CSharpAlgebra
  fClass
  fMembDecl
  fMembMeth
  fStatDecl
  fStatExpr
  fStatIf
  fStatWhile
  fStatReturn
  fStatBlock
  fExprLit
  fExprVar
  fExprOp
  fExprCall

fClass :: ClassName -> [M] -> C
fClass c ms env = (lulGC $ [Bsr "main", UNLINK, HALT] ++ fst (fb' ms env), env)

lulGC :: Code -> Code
lulGC xs = [LINK maxMemory] ++ xs ++ [UNLINK]
  where
    maxMemory = length (filter isMem xs) + 5

    isMem (LDLA _) = True
    isMem (STL _)  = True
    isMem _        = False

fMembDecl :: Decl -> M
fMembDecl d env = ([], env)

-- fMembMeth :: RetType -> Ident -> [Decl] -> (Env -> (Code, Env)) -> M
fMembMeth :: RetType -> Ident -> [Decl] -> S -> M
fMembMeth t x ps s env = ([LABEL x] ++ fst (s subEnv) ++ [RET], sEnv)
  where
    sEnv   = snd sCdEnv
    sCd    = fst sCdEnv
    sCdEnv = s env'
    env'   = updateLabel env
    subEnv = substs 0 sEnv -- update Labels
    -- replacing vars
    substs n senv
      | n == length ps = senv
      | otherwise      = substs (n+1) (subst senv ("MEMB "++x++show n) (ps !! n))

-- replace temp vars with actual var names
subst :: Env -> Ident -> Decl -> Env
subst env old (Decl rt new) = -- trace old $
  let
    v    = snd $ env M.! old
    envD = M.delete old env
    envN = M.insert new (rt, v) envD
  in
    envN

-- each declaration must be added to the environment
-- each declaration must also be put into the mark pointer area
fStatDecl :: Decl -> S
fStatDecl (Decl rt i) env = ([], M.insert i (rt, M.size env + 1) env)

fStatExpr :: E -> S
fStatExpr e env = -- trace ("statExpr " ++ show (M.toList env)) 
  (fst (e Value env') ++ [pop], snd (e Value env'))
  where
    env' = updateLabel env

fStatIf :: E -> S -> S -> S
fStatIf e s1 s2 env = (c ++ [BRF (n1 + 2)] ++ fst (s1 env) ++ [BRA n2] ++ fst (s2 env), env)
 where
  c        = fst $ e Value env
  (n1, n2) = (codeSize (fst (s1 env)), codeSize (fst (s2 env)))

fStatWhile :: E -> S -> S
fStatWhile e s1 env = ([BRA n] ++ fst (s1 env) ++ c ++ [BRT (-(n + k + 2))], env) where
  c = fst $ e Value env
  (n, k) = (codeSize (fst (s1 env)), codeSize c)

fStatReturn :: E -> S
fStatReturn e env = (fst (e Value env) ++ [STR RR] ++ [RET], env)


-- we go back to the old env after the block
-- except we keep the variables involved in method calls 
fStatBlock :: [S] -> S
fStatBlock fes env = -- trace ("block "++show (M.toList fEnv))
  (c, fEnv)
  where
    (c, env') = fb' fes env

    -- making use of the fact that vars dont allow space
    envL = filter (\(x, _) -> take 5 x == "MEMB ") (M.toList env')
    envM = M.fromList envL
    fEnv = updateLabel $ env `M.union` envM

fb' :: [Env -> (Code, Env)] -> Env -> (Code, Env)
fb' []     env = ([], env)
fb' (x:xs) env = let (c, env') = x env in B.first (c ++) (fb' xs env')

fExprLit :: Literal -> E
fExprLit l va env = ([LDC n], env') where
  n = case l of
    LitInt n -> n
    LitBool b -> bool2int b

  env' = updateLabel env

fExprVar :: Ident -> E
fExprVar x va env = case va of
    Value   ->  ([LDL loc], env')
    Address ->  ([LDLA loc], env')
  where loc = snd (env M.! x)
        env' = updateLabel env'

fExprOp :: Operator -> E -> E -> E
fExprOp OpAsg e1 e2 va env = (fst (e2 Value env) ++ [LDS 0] ++ fst (e1 Address env) ++ [STA 0], env')
  where
    env' = updateLabel env
fExprOp OpOr  e1 e2 va env = (fst (e1 Value env') ++ [Brt ("orShort" ++ show n)]
                           ++ [AJS 1] ++ fst (e2 Value env') ++ [OR, Bra ("endOr" ++ show n)]
                           ++ [LABEL ("orShort" ++ show n), AJS 1, LDC (-1), OR, Bra ("endOr" ++ show n)] -- short circuited, we avoid computing (e2 Value env')
                           ++ [LABEL ("endOr" ++ show n)], env')
  where
    n = snd $ env' M.! "No of Labels"
    env' = updateLabel env
fExprOp OpAnd e1 e2 va env = (fst (e1 Value env') ++ [Brf ("andShort" ++ show n)]
                           ++ [AJS 1] ++ fst (e2 Value env') ++ [AND, Bra ("endAnd" ++ show n)]
                           ++ [LABEL ("andShort" ++ show n), AJS 1, LDC 0, AND, Bra ("endAnd" ++ show n)] -- short circuited, we avoid computing (e2 Value env')
                           ++ [LABEL ("endAnd" ++ show n)], env')
  where
    n = snd $ env' M.! "No of Labels"
    env' = updateLabel env
-- the AJS 1's above are because Brt/Brf reduce the stack pointer by 1                                                                              
fExprOp op    e1 e2 va env = (fst (e1 Value env) ++ fst (e2 Value env) ++ [
   case op of
    { OpAdd -> ADD; OpSub -> SUB; OpMul -> MUL; OpDiv -> DIV;
    ; OpMod -> MOD ; OpXor -> XOR;
    ; OpLeq -> LE; OpLt -> LT;
    ; OpGeq -> GT; OpGt -> GT;
    ; OpEq  -> EQ; OpNeq -> NE;}
  ], env')
  where
    env' = updateLabel env

-- Variables are temporarily written as
-- MEMB Ident0, MEMB Ident1,...
-- for function's arguments
-- later replaced
fExprCall :: Ident -> [E] -> E
fExprCall "print" es va env =
    let
      c = concatMap ((++ [LDS 0, TRAP 0]) . fst . ($ env) . ($ va)) es
    in
      (c, env)
fExprCall i       es va env = -- trace ("call!! "++show(M.toList env)) 
    let
      codeEnvs = map (($ env) . ($ va)) es

      addTo :: Int -> Env -> Env
      addTo n env1 = M.insertWith (\_ x -> x) ("MEMB " ++ i ++ show n) (TyVoid, M.size env1 + 1) env1

      addAll 0 env = addTo 0 env
      addAll n env = let env' = addTo n env in addAll (n-1) env'

      fEnv = addAll (length es - 1) env

      lookUpKeys = map (\x -> "MEMB "++ i ++ show x) [0..length es - 1]
      addrs      = map (snd . (fEnv M.!)) lookUpKeys
      instrs     = map ((++ [pop]) . (LDS 0 :) . singleton . STL) addrs
      codes      = map fst codeEnvs
      zipped     = concat $ zipWith (++) codes instrs
    in
      (zipped ++ [Bsr i, LDR RR], fEnv)

-- | Whether we are computing the value of a variable, or a pointer to it
data ValueOrAddress = Value | Address
  deriving Show

-- Encode a C# bool as an int, for the SSM
bool2int :: Bool -> Int
bool2int True  = -1
bool2int False = 0

-- for unique labelling
updateLabel :: M.Map String (RetType, Int) -> M.Map String (RetType, Int)
updateLabel env = env'
  where
    env' = M.insert "No of Labels" (TyVoid, prev+1) env
    prev = snd $ M.findWithDefault (TyVoid, 0) "No of Labels" env