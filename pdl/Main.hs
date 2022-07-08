-- Baseado no código em javascript:
-- 1 - Precisamos quebrar o programa entrada e transformá-lo em uma árvore.
-- 2 - Verificar cada execução da árvore em um grafo para testar a corretude da solução.
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Utils (contains, get, includes, removeChar, split, substring, debug)
import Debug.Trace (traceShow)

entryProgram :: String
entryProgram = "U(;(x?,;(w,y)),;(~x?,z))"

entryProgram1 :: String
entryProgram1 = ";(;(a,b),d)"

entryProgram2 :: [Char]
entryProgram2 = ";(w,z)"

entryProgram3 :: [Char]
entryProgram3 = "*(a)"

entryProgram4 :: [Char]
entryProgram4 = "*(;(a,b))"

entryProgram5 :: [Char]
entryProgram5 = "U(;(a,c),;(a,b))"

operations :: [String]
operations = ["U", ";", "*"]

type Vortex = Int
type Edge = (Vortex, Vortex, Char)
type Graph = [Edge]

graph :: [Edge]
graph =[(1, 2, 'a'), (2, 3, 'c'), (3, 4, 'c')]

graph5 :: [Edge]
graph5 =[(1, 2, 'a'), (2, 3, 'c'), (1, 4, 'c')]

validateGraph :: String -> Graph -> Bool
validateGraph _ [] = False
validateGraph program ((a, b, c) : tail) = do
  program == [c] || validateGraph program tail


-- Itera e devolve a posição inicial
updateStart :: String -> Int -> Int
updateStart input start =
  if not (includes operations (get input start)) && start < length input
    then updateStart input (start + 1)
    else start




-- -------------------------------------- getFirstSubprogram --------------------------------------

-- Itera chamando recursivamente e quebrando a string: while (end < program.length) {
getFirst :: String -> Int -> Int -> Int -> Int -> String
getFirst input opennedBracket closedBracket start end =
  if end < length input then
    if get input end == "(" then
      getFirst input (opennedBracket + 1) closedBracket start (end + 1)
      else if get input end == ")" then
        getFirst input opennedBracket (closedBracket + 1) start (end + 1)
        else if get input end == "," && opennedBracket == closedBracket then
          substring input start end
          else
            getFirst input opennedBracket closedBracket start (end + 1)
  else
     removeChar (removeChar (removeChar (removeChar (removeChar (head (split (==',') input)) '*') 'U') ';') '(') ')'


getFirstSubProgram :: String -> Int -> String
getFirstSubProgram input strStart = do

  let start = updateStart input strStart
  let end = start
  let result = getFirst input 0 0 start end
  result



---------------------------------------- getSecondSubprogram --------------------------------------

-- Itera chamando recursivamente e quebrando a string: while (end < program.length) {
getSecond :: String -> Int -> Int -> Int -> Int -> String
getSecond input opennedBracket closedBracket start end =
  if end < length input then
    if closedBracket >= opennedBracket && opennedBracket /= 0  then
      substring input start end
      else if get input end == "(" then
        getSecond input (opennedBracket+1) closedBracket start (end + 1)
        else if get input end == ")" then
          getSecond input opennedBracket (closedBracket +1) start (end + 1)
          else
            getSecond input opennedBracket closedBracket start (end + 1)
  else
     removeChar (removeChar (removeChar (removeChar (last (split (==',') input)) 'U') ';') '(') ')'


getSecondSubProgram :: String -> Int -> String
getSecondSubProgram input strStart = do

  let start = updateStart input strStart
  let end = start
  let result = getSecond input 0 0 start end
  result


-- Identifica a operação e aplica a recursividade no executeProgram
switch :: String -> Int -> String -> Bool
switch program updatedCursor currentOperator
  | currentOperator == "U" = do
    let paramA = getFirstSubProgram program updatedCursor
    let paramB = getSecondSubProgram program (length paramA + 1)
    executeProgram paramA 0 || executeProgram paramB 0
  | currentOperator == ";" = do
    let paramA = getFirstSubProgram program updatedCursor
    let paramB = getSecondSubProgram program (length paramA + 1)
    executeProgram paramA 0 && executeProgram paramB 0
  | currentOperator == "*" = do
    let paramA = getFirstSubProgram program updatedCursor
    executeProgram paramA 0
  | otherwise = False



-- Executa programa
executeProgram :: String -> Int -> Bool
executeProgram program cursor = do
    let isOperator = includes operations (get program cursor)
    let result = if isOperator
                    then switch program (cursor +1) (get program cursor) -- Chama o switch case de operações e aplica a recursão
                  else do
                      False

    get program (length program -1) == "?" || validateGraph program graph5 || result


-- Início da aplicação
start :: IO ()
start =
  print (executeProgram entryProgram5 0)

