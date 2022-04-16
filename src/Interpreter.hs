module Interpreter
(
    AExp(..),
    BExp(..),
    Com (..),
    aval,
    bval,
    eval
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Machine
import Data.Maybe (fromJust, isNothing)

--TODO Task 2.1
data AExp =
    N Val | V Vname | Plus AExp AExp
    deriving (Eq, Read, Show)

--TODO Task 2.2
aval :: AExp -> State -> Val
aval (N i) _ = i
aval (V n) mem = if isNothing(Map.lookup n mem)
                    then 
                        0
                    else 
                        fromJust (Map.lookup n mem)
                    --fromJust undefined (Map.lookup n mem)
aval (Plus exp1 exp2) st = (+) (aval exp1 st) (aval exp2 st)

--TODO Task 2.1
data BExp =
    Bc Bool | Not BExp | And BExp BExp | Less AExp AExp
    deriving (Eq, Read, Show)

--TODO Task 2.3
bval :: BExp -> State -> Bool
bval (Bc b) _ = b
bval (Not b) st = not (bval b st)
bval (And e1 e2) st = (&&) (bval e1 st) (bval e2 st)
bval (Less e1 e2) st = aval e1 st < aval e2 st

--TODO Task 2.1
data Com =
    Assign Vname AExp | Seq Com Com | If BExp Com Com | While BExp Com | SKIP
    deriving (Eq, Read, Show)

--TODO Task 2.4
eval :: Com -> State -> State
eval (Assign n exp) st = Map.insert n (aval exp st) st
eval (Seq c1 c2) st = eval c2 (eval c1 st)
eval (If b c1 c2) st = if bval b st
                        then eval c1 st
                        else eval c2 st
eval (While b c) st = if bval b st
                        then eval (While b c) (eval c st)
                        else st
eval SKIP st = st