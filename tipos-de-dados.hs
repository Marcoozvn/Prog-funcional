--Escreva a declaracao para o tipo Triple, contendo tres elementos, todos de tipos diferentes.
--Escreva funcoes tripleFst, tripleSnd, tripleThr para extrair respectivamente o primeiro, segundo e terceiro
-- elementos de uma triple.
data Triple a b c = Triple a b c deriving (Eq,Show)

tripleFst (Triple a b c) = a
tripleSnd (Triple a b c) = b
tripleThr (Triple a b c) = c

--Escreva um tipo Quadruple que contem 4 elementos: dois de um mesmo tipo e outros dois de outro tipo
--Escreva as funcoes frstTwo e secondTwo que retornam os dois primeiros e os dois ultimos, respectivamente
data Quadruple a b = Quadruple a a b b deriving (Eq, Show)

firstTwo (Quadruple x y z w) = (x, y)
secondTwo (Quadruple x y z w) = (z, w)

--Escreva um tipo de dados que pode conter um, dois, tres ou quatro elementos, dependendo do construtor
--Implemente funções tuple1 até tuple4 que que retornam Just <valor> ou Nothing se o valor nao existe
data Tuple a b c d = Tuple1 a | Tuple2 a b | Tuple3 a b c | Tuple4 a b c d

tuple1 (Tuple1 a) = Just a
tuple1 (Tuple2 a b) = Just a 
tuple1 (Tuple3 a b c) = Just a
tuple1 (Tuple4 a b c d) = Just a

tuple2 (Tuple2 a b) = Just b
tuple2 (Tuple3 a b c) = Just b
tuple2 (Tuple4 a b c d) = Just b 
tuple2 _ = Nothing

tuple3 (Tuple3 a b c) = Just c
tuple3 (Tuple4 a b c d) = Just c
tuple3 _ = Nothing

tuple4 (Tuple4 a b c d) = Just d 
tuple4 _ = Nothing 

data List a = Nil | Cons a (List a) deriving (Eq,Show)

listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs

listHead Nil = error "Empty list"
listHead (Cons x xs) = x

listTail Nil = error "Empty list"
listTail (Cons x xs) = xs

listFoldr f v Nil = v
listFoldr f v (Cons x xs) = f x (listFoldr f v xs)


listFoldl f v Nil = v
listFoldl f v (Cons x xs) = listFoldl f (f v x) xs 

--Escreva as funcoes sobre a estrutura de dados binary tree
data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a)
 deriving (Eq,Show)

sizeBST NIL = 0
sizeBST (Node a left right) = 1 + sizeBST left + sizeBST right

--verifica se uma BT é uma BST
isBST NIL = False
isBST (Node a left right) = isBSTLeft a left && isBSTRight a right

isBSTLeft x NIL = True
isBSTLeft x (Node a left right) | x < a = False
                                | otherwise = isBSTLeft a left && isBSTRight a right

isBSTRight x NIL = True
isBSTRight x (Node a left right) | x > a = False
                                 | otherwise = isBSTLeft a left && isBSTRight a right
                                
--insere uma nova chave na BST retornando a BST modificada
insert x NIL = Node x NIL NIL
insert x (Node a left right) | x == a = Node x left right
                             | x < a = Node a (insert x left) right
                             | x > a = Node a left (insert x right)

--retorna o Node da BST contendo o dado procurado ou entao NIL
search x NIL = NIL
search x (Node a left right) | x > a = search x right
                             | x < a = search x left
                             | otherwise = (Node a left right)


--retorna o elmento maximo da BST
my_maximum (Node a _ NIL) = a
my_maximum (Node a _ right) = my_maximum right


--retorna o elemento minimo da BST
my_minimum (Node a NIL _) = a
my_minimum (Node a left _) = my_minimum left

--retorna o predecessor de um elemento da BST, caso o elemento esteja na BST
predecessor x (Node a left right) = undefined

--retorna o sucessor de um elemento da BST, caso o elemento esteja na BST
successor = undefined

--remove ume lemento da BST
remove = undefined

--retorna uma lista com os dados da BST nos diversos tipos de caminhamento
preOrder NIL = []
preOrder (Node a left right) = [a] ++ preOrder left ++ preOrder right

order NIL = []
order (Node a left right) = order left ++ [a] ++ order right

postOrder NIL = []
postOrder (Node a left right) = postOrder left ++ postOrder right ++ [a]