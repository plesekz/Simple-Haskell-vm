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
acomp (Plus a b) = acomp a ++ acomp b ++ [ADD]
acomp (N a) = [LOADI a]
acomp (V name) = [LOAD name]

--TODO Task 3.2
bcomp :: BExp -> Bool -> Int -> [Instr]
bcomp (Bc bool) res skip = if bool == res
                            then [JMP skip]
                            else []
bcomp (Not exp) res skip = bcomp exp (not res) skip
bcomp (And exp1 exp2) res skip =
    if res
        then
            bcomp exp1 False (length (bcomp exp2 True skip)) ++ bcomp exp2 True skip
        else
            bcomp exp1 False (length (bcomp exp2 False skip) + skip) ++ bcomp exp2 False skip
    -- if length (bcomp exp2 res skip) == 0 
    --     then bcomp exp1 res skip
    --     else bcomp exp1 (not res) (length (bcomp exp2 res skip))
    -- ++ bcomp exp2 res skip
bcomp (Less aexp1 aexp2) res skip = acomp aexp1 ++ acomp aexp2 ++
                                        if res
                                            then [JMPLESS skip]
                                            else [JMPGE skip]

--TODO Task 3.3
ccomp :: Com -> [Instr]
ccomp (Assign name value) = acomp value ++ [STORE name]
ccomp (Seq seq1 seq2) = ccomp seq1 ++ ccomp seq2
ccomp (If bexp seq1 seq2) = bcomp bexp False (length (ccomp seq1)+1) ++ ccomp seq1 ++ [JMP (length (ccomp seq2))] ++ ccomp seq2
ccomp (While bexp seq) = bcomp bexp False (length (ccomp seq ++ [JMP (negate (length (ccomp seq) +1))])) ++ ccomp seq ++ [JMP (negate (length (bcomp bexp False (length (ccomp seq ++ [JMP (negate (length (ccomp seq) +1))])) ++ ccomp seq) +1))]
ccomp SKIP = []