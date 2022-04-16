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

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isNothing)

--TODO Task 1.1
type Vname = (String)
--TODO Task 1.2
type Val = (Int)
--TODO Task 1.3
type State = (Map String Int)

--TODO Task 1.4
data Instr =
        LOADI Val | LOAD Vname | ADD | STORE Vname | JMP Int | JMPLESS Int | JMPGE Int
        deriving (Eq, Read, Show)

--TODO Task 1.5
type Stack = ([Int])

--TODO Task 1.6
type Config = (Int, State, Stack)

--TODO Task 1.7
iexec :: Instr -> Config -> Config
iexec (LOADI i) (counter, mem, st) = (counter+1, mem, i:st)
iexec (LOAD v) (counter, mem, st) = (counter+1, mem,
                                        if isNothing (Map.lookup v mem)
                                                then 0:st
                                                else fromJust (Map.lookup v mem):st

                                    )
iexec (ADD) (counter, mem, st) = (counter+1, mem, add st)
iexec (STORE v) (counter, mem, st) = (counter+1, Map.insert v (head st) mem , drop 1 st)
iexec (JMP i) (counter, mem, st) = (counter+1+i, mem, st)
iexec (JMPLESS i) (counter, mem, st) = if (head st > head (drop 1 st))
                                        then (counter+1+i, mem, (drop 2 st))
                                        else (counter+1, mem, (drop 2 st))
iexec (JMPGE i) (counter, mem, st) = if (head (drop 1 st) > head st)
                                        then (counter+1+i, mem, (drop 2 st))
                                        else (counter+1, mem, (drop 2 st))

--TODO Task 1.8
exec :: [Instr] -> Config -> Config
exec i (counter, mem, st) = if(getCounter(iexec (head(drop counter i)) (counter, mem, st)))==(length i)
                                then
                                        iexec (head(drop counter i)) (counter, mem, st)
                                else
                                        exec i (iexec (head(drop counter i)) (counter, mem, st))
        -- iexec (head(drop counter i)) (counter, mem, st)

getCounter :: Config -> Int
getCounter (counter, mem, st) = counter

add :: Stack -> Stack
add st = (head st + head(drop 1 st)):(drop 2 st)