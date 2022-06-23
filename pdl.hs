{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

split :: [Char] -> [[Char]]
split str = case break (=='(') str of
                (a, '(':b) -> a : "(" : split b
                (a, "")    -> [a]

head :: [Char] -> Char
head (x:_) = x


getOperations program = do
  let splitedArray = split program
  let operator = Prelude.head splitedArray
  print operator
  print splitedArray
  -- getOperations program


main :: IO ()
main = do
    program <- getLine
    getOperations program


-- if x then a else b
-- (X?;a) U (~X?;b)
-- ;(b,a)
-- a d c
