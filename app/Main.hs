module Main where

import System.Environment
import Compiler
import Interpreter
import System.Environment (getArgs)

--TODO Task 3.4
main :: IO ()
main = do
    args <- getArgs
    let a = head args
    print(ccomp (read (a)::Com))



{-

--parser
main :: IO ()
main = do
    args <- getArgs
    --print args
    --print(merge args)
    --print(ppS (merge args))
    --print(f (gFW (ppS (merge args))))
    --print(process (merge args))
    print(ccomp (process (merge args)))

merge :: [String] -> String
merge [x] = x
merge (x:xs) = x ++ [' ']  ++ merge xs

process :: String -> Com
process arg = f(parseCom (f t) (l t))
    where t = gFW (ppS arg)

ppS :: String -> String
ppS xs = [ x | x <- xs, x `notElem` "()" ]


parseCom :: String -> String -> (Com, String)
parseCom "Assign" s = (Assign (f (gFW s)) (f (parseAEXP (f (gFW (l (gFW s)))) (l (gFW (l (gFW s)))))), l (parseAEXP (f (gFW (l (gFW s)))) (l (gFW (l (gFW s))))))
parseCom "Seq" s = (Seq
                        (f (ppCom s))
                        (f (ppCom (l (ppCom s)))
                        )
                    , l (ppCom (l (ppCom s))))
parseCom "If" s = (If
                    (f (ppBex s))
                    (f (ppCom (l (ppBex s))))
                    (f (ppCom (l (ppCom (l (ppBex s))))))
                , l (ppCom (l (ppCom (l (ppBex s))))))
parseCom "While" s = (While
                        (f (ppBex s))
                        (f (ppCom (l (ppBex s))))
                        , l (ppCom (l (ppBex s))))
parseCom "SKIP" s = (SKIP, s)

parseBEXP :: String -> String -> (BExp, String)
parseBEXP "Bc" s = (Bc (f (ppBool s)), l (ppBool s))
parseBEXP "Not" s = (Not (f (ppBex s))
                    ,l (ppBex s))
parseBEXP "And" s = (And (f (ppBex s)) (f(ppBex (l (ppBex s)))), l(ppBex (l (ppBex s))))
parseBEXP "Less" s = (Less (f (ppAexp s)) (f(ppAexp (l (ppAexp s)))), l(ppAexp (l (ppAexp s))))

parseAEXP :: String -> String -> (AExp, String)
parseAEXP "N" s = (N (read (f (gFW s)) :: Int), l (gFW s))
parseAEXP "V" s = (V (f (gFW s)), l (gFW s))
parseAEXP "Plus" s = (Plus (f (ppAexp s)) (f (ppAexp (l (ppAexp s)))), l (ppAexp (l (ppAexp s))))

parseBool :: String -> Bool
parseBool "True" = True
parseBool "False" = False

ppCom :: String -> (Com, String)
ppCom s = parseCom (f (gFW s)) (l (gFW s))

ppBex :: String -> (BExp, String)
ppBex s = parseBEXP (f (gFW s)) (l (gFW s))

ppAexp :: String -> (AExp, String)
ppAexp s = parseAEXP (f (gFW s)) (l (gFW s))

ppBool :: String -> (Bool, String)
ppBool s = (parseBool (f (gFW s)), l (gFW s))

f :: (a,b) -> a
f (a, b) = a

l :: (a,b) -> b
l (a, b) = b

fS :: String -> Int --find (first) Space char in a String
fS [] = 0
fS s = if head s == ' '
    then 0
    else fS (drop 1 s)+1

gFW :: String -> (String, String) --give First Word
gFW s = (take (fS s) s , drop (fS s + 1) s)

-- each parse function takes the first word, the word to be parsed, and then parses second (and other) words as required by the keyword. It returns a touple consisting of the parsed expression and remaining 
-- string to be parsed

--Plus (N 1) (N 2)

-}