type Simbolo = Int
type Estado = Int
type Entrada = [Simbolo]
type Final = Estado
type Transicao = (Simbolo, Estado, Estado) {-Seja (x,y,z) : Ao ler o simbolo X no estado Y vai para o estado Z-}

isFinal :: Estado -> [Estado] -> Bool {-confere se é um estado final-}
isFinal _ [] = False
isFinal x (a:as) = if (x == a) then True else (isFinal x as)

fazTransicao :: (Simbolo, Estado) -> [Transicao] -> Estado {-Seja (x,y) o simbolo atual e estado atual. Confere na lista de transicoes e vai para o proximo estado-}
fazTransicao _ [] = -1
fazTransicao (x, y) ((a, b, c):as) = if (x==a && y==b) then c else (fazTransicao (x,y) as)

simula :: Entrada -> Estado -> [Estado] -> [Transicao] -> [Final] -> Bool {-faz a simulacao	-}
simula [] q _ _ fs = isFinal q fs
simula (x:xs) q qs ts fs = simula xs (fazTransicao (x,q) ts) qs ts fs

{-
Como testar no ghci:
simula recebe: Entrada (Lista de ints), Estado atual, Lista de estados do autômato, Lista de transições do autômato, lista de estados finais do automato
retorna o booleano dizendo se a cadeia foi aceita ou não.

No ghci, quando for testar, passe como segundo argumento o estado inicial (como estado atual).

Esse autômato reconhece cadeias de tamanhos pares:
estados: [0,1]
transicoes: [(0,0,1), (0,1,1), (1,0,0), (1,1,0)]
estados finais: [1]
estado inicial: 0

Para testar a cadeia "0101", por exemplo 
simula [0,1,0,1] 0  [0,1] [(0,0,1), (0,1,1), (1,0,0), (1,1,0)] [0]

Esse autômato reconhece se uma cadeia não é vazia:
estados: [0,1]
transicoes: [(0,0,1), (0,1,1), (1,0,1), (1,1,1)]
finais: [0]
estado inicial: 0

Para testar a cadeia "0101", por exemplo 
simula [0,1,0,1] 0  [0,1] [(0,0,1), (0,1,1), (1,0,1), (1,1,1)] [0]
-}