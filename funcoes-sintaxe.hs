{-
- Dada uma tupla, divide o primeiro pelo segundo usando pattern matching.
- Ela deve ser indefinida quando o denominador for zero.
-}
divTuple (x, 0) = undefined
divTuple(x, y) = x/y

{-
 - Calcula o somatorio entre dois numeros a e b (a < b). Procure usar alguma funcao pronta sobre listas. 
 - Ex: somatorio 0 1 = 1
 -     somatorio 1 3 = 6
-}
somatorio a b = sum [a..b]

{-
 - Calcula o somatorio (recursivo) entre dois numeros a e b (a < b).
 - Ex: somatorio 0 1 = 1
 -     somatorio 1 3 = 6
-}
somatorioRec a b = somatorioRec' [a..b]

somatorioRec' [] = 0
somatorioRec' (x:xs) = x + somatorioRec' xs 

-- Defina a funcao que eleva um membro ao quadrado
square x = x**2

-- Soma os quadrados de dois numeros.
sumSquares x y = square x + square y

-- Defina uma funcao de alta ordem que aceita uma função (Int -> Int) e aplica a funcao a dois numeros
higherOrderSum f a b = sum (map f [a,b])

-- Defina a soma dos auqdrados em termos de higherOrderSum
hoSumSquares = higherOrderSum square

--Implemente a funcao mapFilter que primeiro aplica o map de uma funcao f a uma lista e depois aplica a funcao filter
-- a lista resultante. Procure usar a composicao de funcoes
mapFilter f p xs = filter p (map f xs)
