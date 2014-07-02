data Expr = Lit Int | Add Expr Expr | Sub Expr Expr | Mult Expr Expr | Div Expr Expr | Mod Expr Expr

eval :: Expr -> Int
eval (Lit n) = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Sub e1 e2) = (eval e1) - (eval e2)
eval (Mult e1 e2) = (eval e1) * (eval e2)
eval (Div e1 e2) = div (eval e1) (eval e2)	
eval (Mod e1 e2) = mod (eval e1) (eval e2)

{-
A forma de representação escolhida foi:
Lit Int : para representar o construtor de um inteiro, Int x (tal que x é o inteiro)
Add Expr Expr : para representar uma adição entre o valor de duas expressões
Sub Expr Expr : para representar uma subtração entre o valor de duas expressões
Mult Expr Expr : para representar uma multiplicação entre o valor de duas expressões
Div Expr Expr : para representar uma divisão entre o valor de duas expressões
Mod Expr Expr : para representar o resto da divisão entre o valor de duas expressões

Como testar:
ex1: (1 + 4) * 2
eval $ Mult (Add (Lit 1) (Lit 4)) (Lit 2)

ex2: (20 % 13) * ((3 + (4 -2)) + 1)
eval $ Mult (Mod (Lit 20) (Lit 13)) $ Add (Add (Lit 3) $ Sub (Lit 4) (Lit 2)) (Lit 1) 

ex3: ( 5 + ( 3 - ( 3 * ( 10 / (4 + 3) ) ) ) )
eval $ Add (Lit 5) $ Sub (Lit 3) $ Mult (Lit 3) $ Div (Lit 10) $ Add (Lit 4) (Lit 3)

ex4: 1 + ( (5 % 2) / (5 * 3) )
eval $ Add (Lit 1) $ Div (Mod (Lit 5) (Lit 2)) (Mult (Lit 5) (Lit 3))

ex5: (5+3)*(10-4)
eval $ (Mult (Add (Lit 5) (Lit 3)) (Sub (Lit 10) (Lit 4)))
-}