-- Baseado no código em javascript:

-- 1 - Precisamos quebrar o programa entrada e transformá-lo em uma árvore.
-- 2 - Verificar cada execução da árvore em um grafo para testar a corretude da solução.

import Utils (substring, includes, contains, get, split, removeChar)


-- Variáveis globais (Na verdade, são funções :/ )
program :: String
program = "U(;(x?,;(w,y)),;(~x?,z))"

operations :: [String]
operations = ["U", ";", "*"]


-- Executa programa
executeProgram :: String -> Int -> Bool
executeProgram program cursor = do
    let isOperator = includes operations (get program cursor)
    let aux = if isOperator
        then do
            let currentOperator = get program cursor
            let updatedCursor = cursor +1

            -- Tem que seguir daqui :)
            let switch currentOperator
                  | currentOperator == ";" = "You got a A"
                  | currentOperator == "U" = "you got a B"
                  | currentOperator == "*" = "You got a C"
                  | otherwise = "You got a F"

            True
        else do
            let operation = get program cursor
            False
    False



-- Início da aplicação
start :: IO ()
start = do
    print program -- Teste, deve ser removido

    print (get program 0) -- Teste, deve ser removido
    print(includes operations (get program 0)) -- Teste, deve ser removido
    print(split (==',') ";(x,y)" ) -- Teste, deve ser removido
    print (removeChar "teste" 'e') -- Teste, deve ser removido
    print(executeProgram program 0)
    -- print (executeProgram program 0)