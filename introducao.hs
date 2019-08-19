{-
- Usando os predicados not,and e or prontos de Haskell, implemente os predicados (funcoes) xor (or exclusivo),
- impl (implicacao A => B é equivalente a (not A or B)) e equiv (A <=> B é definido como A => B and B => A)
- Procure usar casamento de padroes e reutilizar as funcoes.
-}
xor a b = (a && not b) || (b && not a)
impl a b = not a || b
equiv a b = (impl a b ) && (impl b a)

{-
A funcao square esta implementada e eleva ao quadrado um determinado numero
-}
square x = x*x

{-
- Implemente a funcao potencia, que retorna o resultado de x elevado a y 
-}
pow x y = x ** y

{-
- Implemente a funcao fatorial que calcula o fatorial de um numero 
-}
fatorial 0 = 1
fatorial x = x * fatorial (x-1)

{-
- Determina se um numero eh primo ou nao. Preocupe-se apenas em resolver o problema.
- Nao precisa usar conhecimentos mais sofisticados da teoria dos numeros. Voce pode trabalhar com listas.
-}
isPrime 1 = True
isPrime 2 = True
isPrime x = isPrime' x [2..(x-1)]

isPrime' x [] = True
isPrime' x (y:ys) | mod x y == 0 = False
                  | otherwise = isPrime' x ys

{-
- Calcula um termo da sequencia de Fibonnacci. Voce pode trabalhar com listas. 
-}

fib 1 = 1
fib 2 = 1
fib x = (fib (x-1)) + (fib (x-2))

{-
- Calcula um MDC de dois numeros usando o algoritmo de Euclides. 
-}
mdc 0 y = y
mdc x 0 = x
mdc x y = mdc' y (mod x y)

mdc' q r | mod q r == 0 = r
         | otherwise = mdc' r (mod q r)

{-
- Calcula um MMC de dois numeros. 
-}
mmc x y = head ys 
          where ys = filter (divisivel x y) [(min x y)..x*y]
divisivel x y n = mod n x == 0 && mod n y == 0

{-
- Determina se dois numeros inteiros positivos sao co-primos. Dois numeros sao co-primos se 
- o mdc deles for igual a 1. Ex: coprimo 35 64 = True 
-}
coprimo x y | mdc x y == 1 = True
            | otherwise = False

{-
- Calcula a conjectura de Goldbach, que diz que um numero par maior que 2 pode ser escrito como a soma de dois numeros primos. Ex: 28 = 5 + 23.
-}
goldbach x = [ (y, z) | y <- filter isPrime [1..(x-1)], z <- filter isPrime [1..(x-1)], y + z == x]
