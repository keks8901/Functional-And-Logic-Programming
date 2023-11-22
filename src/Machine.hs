module Machine
(      
        Vname,
        Val,
        State,
        Instr (..),
        Stack,
        Config,
        iexec,
        exec
) where

import Data.Map

--TODO Task 1.1
type Vname = [Char] 

--TODO Task 1.2
type Val = Int

--TODO Task 1.3
type State = Map Vname Val 

--TODO Task 1.4
data Instr = LOADI Val
        | LOAD Vname 
        | ADD
        | STORE Vname
        | JMP Val
        | JMPLESS Val 
        | JMPGE Val
        deriving (Eq, Read, Show)
        
--TODO Task 1.5
type Stack = [Int]

--TODO Task 1.6
type Config = (Int, State, Stack)

--TODO Task 1.7
iexec :: Instr -> Config -> Config
--
iexec (LOADI x) (i, state, stack) = (i+1, state, x : stack)
--
iexec (LOAD v) (i, state, stack) = (i+1, state, head[elems(state)]++stack)
--
iexec (ADD) (i, state, stack) = (i+1, state, [sum (Prelude.take 2 stack)] ++ Prelude.drop 2 stack)
--
iexec (STORE v) (i, state, s:stack) = (i+1, insert v s (state), Prelude.drop 1 stack ++ stack)
--
iexec (JMP i) (ii, state, stack) = (ii+i+1, state, stack)
--
iexec (JMPLESS i) (ii, state, stack) = 
        if stack!!0 > stack!!1 then (ii+i+1, state, Prelude.drop 2 stack)
        else (ii+1, state, Prelude.drop 2 stack)
--
iexec (JMPGE i) (ii, state, stack) = 
        if stack!!0 <= stack!!1 then (ii+i+1, state, Prelude.drop 2 stack)
        else (ii+1, state, Prelude.drop 2 stack)

--TODO Task 1.8
exec :: [Instr] -> Config -> Config
exec [] conf = conf
exec (inst:insts) conf = exec insts (iexec inst conf)
