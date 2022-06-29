{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- split :: [Char] -> [[Char]]
-- split str = case break (=='(') str of
--                 (a, '(':b) -> a : "(" : split b
--                 (a, "")    -> [a]

-- head :: [Char] -> Char
-- head (x:_) = x


-- getOperations program = do
--   let splitedArray = split program
--   let operator = Prelude.head splitedArray
--   print operator
--   print splitedArray
  -- getOperations program



type Estados = Int
type Letra = Char
type Aresta = (Estados, Estados,Letra)
type Grafo = [Aresta]
type Operacoes = Char


operacoes :: Operacoes
operacoes = ";"

grafo :: [Aresta]
grafo = [
        (1,2,"A"), (2,3,"B")
    ]

programa :: Grafo -> Operacoes -> [Int]
programa [] _  = []
programa ((a,b,c):d) oper
        | (";"== oper) = b:(programa d oper)