-- Baseado no código em javascript:
-- 1 - Precisamos quebrar o programa entrada e transformá-lo em uma árvore.
-- 2 - Verificar cada execução da árvore em um grafo para testar a corretude da solução.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant bracket" #-}

import System.IO (hPrint)
import Utils (contains, get, includes, removeChar, split, substring)

-- Variáveis globais (Na verdade, são funções :/ )
-- program :: String
-- program = "U(;(x?,;(w,y)),;(~x?,z))"

-- program = "U(;(a,b),U(;(b,a),(;(a,b))))"

-- program = ";(a,b),;(a,b)"
program :: String
program = "U(;(a,b),;(*(a),b))"

operations :: [String]
operations = ["U", ";", "*"]

type Vertice = Int

type Aresta = (Vertice, Vertice, Char)

type Grafo = [Aresta]

type Letras = [Char]

letras :: Letras
letras = ['a' .. 'z']

grafo :: [Aresta]
grafo =
  [ (1, 2, 'w'),
    (1, 3, 'b'),
    (1, 4, 'b')
  ]

validateGraph :: Grafo -> String -> Int -> Bool
validateGraph [] _ _ = False
validateGraph ((a, b, c) : tale) program start = do
  let result = get program start
  if ((get program start) == [c])
    then True
    else validateGraph tale program (start + 1)

getFirstSubProgram :: String -> Int -> Int -> Int -> Int -> String
getFirstSubProgram program start end opennedBracket closedBracket = do
  -- aux program operations = do
  --     if(not(includes program operations)) then
  --         aux program (start + 1) (end + 1) opennedBracket closedBracket

  if ((get program end) == "(")
    then getFirstSubProgram program start (end + 1) (opennedBracket + 1) (closedBracket)
    else
      if ((get program end) == ")")
        then getFirstSubProgram program start (end + 1) (opennedBracket) (closedBracket + 1)
        else
          if ((get program end) == "," && (opennedBracket == closedBracket))
            then substring program start end
            else -- else
            --   if ((get program end) == "," && (opennedBracket > 1) && (closedBracket > 1) && (opennedBracket - closedBracket == 1))
            --     then substring program start end
            --     else
            --       if ((get program end) == "")
            --         then substring program start end
            -- else
            --   if (((get program end) /= "U") && ((get program end) /= ";") && ((get program end) /= "*") && (closedBracket == opennedBracket))
            --     then getFirstSubProgram program (start + 1) (end + 1) opennedBracket closedBracket
            --     else
            --       if (((get program end) /= "U") && ((get program end) /= ";") && ((get program end) /= "*") && (closedBracket < opennedBracket))
            --         then getFirstSubProgram program start (end + 1) opennedBracket closedBracket
              getFirstSubProgram program start (end + 1) (opennedBracket) (closedBracket)

-- TODO caso base getFirstSubProgram

getSecondSubProgram :: String -> Int -> Int -> Int -> Int -> String
getSecondSubProgram program start end opennedBracket closedBracket = do
  -- aux program operations = do
  --     if(not(includes program operations)) then
  --         aux program (start + 1) (end + 1) opennedBracket closedBracket

  if ((get program end) == "(")
    then getSecondSubProgram program start (end + 1) (opennedBracket + 1) (closedBracket)
    else
      if ((get program end) == ")")
        then getSecondSubProgram program start (end + 1) (opennedBracket) (closedBracket + 1)
        else
          if ((get program end) == "," && (opennedBracket == closedBracket))
            then substring program start end
            else
              if ((get program end) == "," && (opennedBracket > 1) && (closedBracket > 1) && (opennedBracket - closedBracket == 1))
                then substring program start end
                else
                  if ((get program end) == "")
                    then substring program start end
                    else -- else
                    --   if (((get program end) /= "U") && ((get program end) /= ";") && ((get program end) /= "*") && (closedBracket == opennedBracket))
                    --     then getSecondSubProgram program (start + 1) (end + 1) opennedBracket closedBracket
                    --     else
                    --       if (((get program end) /= "U") && ((get program end) /= ";") && ((get program end) /= "*") && (closedBracket < opennedBracket))
                    --         then getSecondSubProgram program start (end + 1) opennedBracket closedBracket
                      getSecondSubProgram program start (end + 1) (opennedBracket) (closedBracket)

-- TODO caso base  getSecondSubProgram

-- Executa programa
executeProgram :: String -> Int -> Bool
executeProgram program cursor = do
  let isOperator = includes operations (get program cursor)
  let aux =
        if isOperator
          then do
            let currentOperator = get program cursor
            let updatedCursor = cursor + 1
            -- Tem que seguir daqui :)
            let switch currentOperator
                  | currentOperator == ";" = do
                    let paramA = getFirstSubProgram program updatedCursor updatedCursor 0 0 --program start end  opennedBracket closedBracket
                    let paramB = getSecondSubProgram program (length paramA + 1) (length paramA + 1) 0 0 --program, (start + paramA.length), end, opennedBracket, closedBracket
                    -- if((validateGraph grafo paramA 0) && (validadeGraph grafo paramB 0)) then
                    --     True
                    -- else
                    (executeProgram paramA 0) && (executeProgram paramB 0)
                  | currentOperator == "U" = do
                    let paramA = getFirstSubProgram program updatedCursor updatedCursor 0 0 --program start end opennedBracket closedBracket
                    let paramB = getSecondSubProgram program (length paramA + 1) (length paramA + 1) 0 0 --program, (start + paramA.length), end, opennedBracket, closedBracket
                    (executeProgram paramA 0) || (executeProgram paramB 0)
                  | currentOperator == "*" = do
                    let paramA = getFirstSubProgram program updatedCursor updatedCursor 0 0 --program start end  opennedBracket closedBracket
                    let paramB = getSecondSubProgram program updatedCursor cursor 0 0 --program, (start + paramA.length), end, opennedBracket, closedBracket
                    (executeProgram paramA 0)
                  | otherwise = False

            True
          else do
            let operation = get program cursor
            False

  -- if(includes program '?') then
  --     True

  --let needRemove = -1

  --loop para verificar o programa TODO: verificar se o programa é válido

  -- if(needRemove > -1)
  --     removeChar program needRemove 1
  --     True
  aux

-- Início da aplicação
start :: IO ()
start = do
  print program -- Teste, deve ser removido
  print (get program 0) -- Teste, deve ser removido
  -- print(includes operations (get program 0)) -- Teste, deve ser removido
  -- print(split (==',') ";(x,y)" ) -- Teste, deve ser removido
  --print (removeChar "teste" 1) -- Teste, deve ser removido
  print (getFirstSubProgram program 0 0 0 0)
  print (getSecondSubProgram program 15 15 0 0)

  -- print (substring "nome" 0 4)

  --   print (getFirstSubProgram "U(;(x?,;(w,y)),;(~x?,z))" 1 1 0 0 False) -- Teste, deve ser removido
  print (executeProgram program 0)

-- print (executeProgram program 0)