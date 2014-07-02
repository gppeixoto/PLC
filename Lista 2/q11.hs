data Tree t = Node t (Maybe (Tree t)) (Maybe (Tree t)) deriving (Show, Eq)

{-
I)inorder :: (Ord t) => Maybe (Tree t) -> [t], que deve percorrer a árvore da esquerda pra direita,
na ordem: lado esquerdo, valor do nó e lado direito;

II)postorder :: (Ord t) => Maybe (Tree t) -> [t], que deve percorrer a árvore da direita pra
esquerda, vendo o lado esquerdo, depois o lado esquerdo e depois o valor do nó;

III)preorder :: (Ord t) => Maybe (Tree t) -> [t], que deve percorrer a árvore da esquerda para a
direita, mas verificando o valor do nó antes do seu lado esquerdo;

IV)breadthfirstorder :: (Ord t) => Maybe (Tree t) -> [t], que deve percorrer a árvore e retornar a
lista de nós da esquerda para a direita, mas agrupados do menos profundo para o mais
profundo;

-}

inorder :: (Ord a) => Maybe (Tree a) -> [a]
inorder Nothing = []
inorder (Just (Node n t1 t2)) = inorder t1 ++ [n] ++ inorder t2

postorder :: (Ord t) => Maybe (Tree t) -> [t]
postorder Nothing = []
postorder (Just (Node a b c)) = (postorder b) ++ (postorder c) ++ [a]

preorder :: (Ord t) => Maybe (Tree t) -> [t]
preorder Nothing = []
preorder (Just (Node a b c)) = [a] ++ (preorder b) ++ (preorder c)

breadthfirstorder :: (Ord t) => Maybe (Tree t) -> [t]
breadthfirstorder Nothing = []
breadthfirstorder (Just (Node val left right))
 | (left == Nothing && right == Nothing) = [val]
 | 