-- Utils.hs será responsável por todas as funções auxiliares de manipulação de string (Todas que usamos no code javascript)


module Utils (substring, contains, includes, get, split, removeChar, debug) where
import Debug.Trace

-- Retorna uma substring com base na string informada e no índice inicial e final
substring :: String -> Int -> Int -> String
substring str start end = take (end - start) (drop start str)


-- Verifica se uma string está contida em outra - Acho que nem vai precisar
contains :: String -> String -> Bool
contains str substr = length str >= length substr && (take (length substr) str == substr)


-- Verifica se uma string está na lista de strings (Para checar as operações)
includes :: [String] -> String -> Bool
includes [] _ = False
includes (x:xs) str = (x == str) || includes xs str


-- Retorna a string no index informado (Para trabalhar como se a string fosse um array - Igual fiz em JS)
get :: String -> Int -> String
get str index = substring str index (index + 1)


-- Retorna uma lista de strings com base no delimitador informado
-- Chamar dessa forma: split (==',') ";(x,y)" 
split     :: (Char -> Bool) -> String -> [String]
split p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : split p s''
                            where (w, s'') = break p s'

-- Remove um caractere da string
removeChar :: String -> Char -> String
removeChar [] _ = []
removeChar (x:xs) n = if x == n then removeChar xs n
                 else x : removeChar xs n


debug = flip trace
