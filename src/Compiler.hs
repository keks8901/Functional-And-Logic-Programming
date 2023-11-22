module Compiler
(
    acomp,
    bcomp,
    ccomp
) where

import Machine
import Interpreter

--TODO Task 3.1
acomp :: AExp -> [Instr]
acomp (N a) = [LOADI a]
acomp (V b) = [LOAD b]
acomp (Plus a b) = acomp(a)!!0 : acomp(b)!!0 : ADD : []

--TODO Task 3.2
bcomp :: BExp -> Bool -> Int -> [Instr]
bcomp (Bc bo) b i = 
    if bo == b then JMP i:[]
    else []

bcomp (Not (Bc bo)) b i = 
    if not(bo) == b then JMP i:[]
    else []

bcomp (And (Bc bo1) (Bc bo2)) b i = 
    if (bo2 && bo1) == b then JMP i:[]
    else []

bcomp (And (Bc bo1) bo2) b i = 
    if bo1 == b then JMP i: bcomp(bo2) b i
    else bcomp(bo2) b i

bcomp (And bo1 (Bc bo2)) b i = 
    if bo2 == b then JMP i:bcomp(bo1) b i
    else bcomp(bo1) b i

bcomp (Less v n) b i = 
    if b == True then acomp(v)!!0:acomp(n)!!0:JMPLESS i : []
    else acomp(v)!!0:acomp(n)!!0:JMPGE i : []

--TODO Task 3.3
ccomp :: Com -> [Instr]
ccomp (Assign v a) = acomp(a) ++ [STORE v]
ccomp (Seq com1 com2) = ccomp(com1) ++ ccomp(com2)

ccomp (If bo com1 com2) = bcomp (bo) False (length(ccomp(com1))+1)  ++ ccomp(com1) ++ [JMP (length(ccomp(com2)))] ++ ccomp(com2)

ccomp (While bo com) = bcomp (bo) False (length(ccomp(com))+1) ++ ccomp(com) ++ [JMP (-(length(ccomp((While bo com)))))]