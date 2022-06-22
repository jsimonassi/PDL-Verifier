
-- Dois primeiros parâmetros são entrada. O último na arrow é sempre o retorno
mySum :: Float -> Float -> Float
mySum a b = a + b

mySubtract :: Float -> Float -> Float
mySubtract a b = a - b

myMultiply :: Float -> Float -> Float
myMultiply a b = a * b

myDivision :: Float -> Float -> Float
myDivision a b = a / b

mySqrt :: Float -> Float
mySqrt = sqrt


main = do
    print (mySum 1 2)
    print (mySubtract 1 2)
    print (myMultiply 1 2)
    print (myDivision 1 2)
    print (mySqrt 16)