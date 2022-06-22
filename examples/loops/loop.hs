-- Define caso base da recursão
loop 0 = return ()

-- Demais casos, faça:
loop n = do
  print n
  loop (n-1)

main = loop 10