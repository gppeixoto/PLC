type Nome = String
type Mana = Int
type Ataque = Int
type Vida = Int
type DanoCausado = Int

data Card = Lacaio Nome Mana Ataque Vida | Feitico Nome Mana DanoCausado deriving (Show, Eq)
data Jogador = Jg Nome [Card] Mana Vida deriving (Show, Eq)
data LacaioCampo = Lc Nome Mana Ataque Vida deriving (Show , Eq)
data Campo = Cmp [LacaioCampo] [LacaioCampo] deriving (Show, Eq)

removeOne :: (Eq t) => t -> [t] -> [t] --remove todas as ocorrencias de um elemento x de uma lista
removeOne _ [] = []
removeOne a (x:xs) = if (a == x) then removeOne a xs else [x] ++ removeOne a    xs

removeAll :: (Eq t) => [t] -> [t] -> [t] --remove uma lista de outra lista (ex: removeAll [1,2,3] [1..10] retorna [4..10])
-- esse metodo sera usado para remover os cards da mão do jogador após serem usadas
removeAll [] l = l
removeAll (a:as) l = removeAll as (removeOne a l)

manaDropFeitico :: [Card] -> Mana -> Mana --reduz a mana do jogador de acordo com os feiticos a serem usados
manaDropFeitico [] m = m
manaDropFeitico ((Feitico n mf dc):fs) m = manaDropFeitico fs (m - mf)

manaDropLacaio :: [Card] -> Mana -> Mana --reduz a mana do jogador de acordo com os lacaios a serem postos em campo
manaDropLacaio [] m = m
manaDropLacaio ((Lacaio n mf dc v):fs) m = manaDropLacaio fs (m - mf)

lacaios :: [Card] -> [Card] --filtra os lacaios de uma lista de cards
lacaios [] = []
lacaios ((Feitico n m dc):cs) = lacaios cs
lacaios ((Lacaio n m a v):cs) = (Lacaio n m a v):(lacaios cs)

feiticos :: [Card] -> [Card] --filtra os feiticos de uma lista de cards
feiticos [] = []
feiticos ((Feitico n m dc):cs) = ((Feitico n m dc):(feiticos cs))
feiticos ((Lacaio n m a v):cs) = feiticos cs

lacaioParaCampo :: [Card] -> [LacaioCampo] --recebe uma lista de lacaios e os coloca em campo
lacaioParaCampo [] = []
lacaioParaCampo ((Feitico n m dc):cs) = lacaioParaCampo cs
lacaioParaCampo ((Lacaio n m a v):cs) = (Lc n m a v):(lacaioParaCampo cs)

danoLacaioCampo :: [LacaioCampo] -> Vida -> Vida --calcula o dano a ser causado no oponente com os lacaios que estao em campo
danoLacaioCampo [] v = v
danoLacaioCampo ((Lc nome mana ataque vida):ls) v = danoLacaioCampo ls (v - ataque)

danoFeitico :: [Card] -> Vida -> Vida --calcula o dano a ser causado no oponente com os feiticos que serao usados
danoFeitico [] v = v
danoFeitico ((Feitico n m dc):cs) v = danoFeitico cs (v - dc)

popularCampo1 :: [LacaioCampo] -> Campo -> Campo --recebe uma lista de lacaios de campo e os coloca no campo do jogador 1
popularCampo1 [] cmp = cmp
popularCampo1 ls (Cmp lc1 lc2) = (Cmp (lc1 ++ ls) lc2)

popularCampo2 :: [LacaioCampo] -> Campo -> Campo --recebe uma lista de lacaios de campo e os coloca no campo do jogador 2
popularCampo2 [] cmp = cmp
popularCampo2 ls (Cmp lc1 lc2) = (Cmp lc1 (lc2 ++ ls))

pickCards :: Mana -> [Card] -> [Card] --politica de selecao das cartas, o jogador vai usar as cartas que "der pra usar" na ordem que aparecer na sua mao,
--independentemente se é lacaio ou feitiço. Ou seja, dada a lista de cartas, filtra as cartas que poderao ser utilizadas
pickCards mana [] = []
pickCards mana ((Feitico n m dc):cs) = if ((mana - m) >= 0) then (Feitico n m dc):(pickCards (mana - m) cs) else (pickCards mana cs)
pickCards mana ((Lacaio n m a v):cs) = if ((mana - m) >= 0) then (Lacaio n m a v):(pickCards (mana - m) cs) else (pickCards mana cs)

ataqueLacaios1 :: (Jogador, Jogador, Campo) -> (Jogador, Jogador, Campo) --os lacaios em campo do jogador 1 causam dano no jogador 2
ataqueLacaios1 ((Jg nome1 cs1 m1 v1), (Jg nome2 cs2 m2 v2), (Cmp lc1 lc2)) = ((Jg nome1 cs1 m1 v1), (Jg nome2 cs2 m2 (danoLacaioCampo lc1 v2)), (Cmp lc1 lc2))

ataqueLacaios2 :: (Jogador, Jogador, Campo) -> (Jogador, Jogador, Campo) --os lacaios em campo do jogador 2 causam dano no jogador 1
ataqueLacaios2 ((Jg nome1 cs1 m1 v1), (Jg nome2 cs2 m2 v2), (Cmp lc1 lc2)) = ((Jg nome1 cs1 m1 (danoLacaioCampo lc2 v1)), (Jg nome2 cs2 m2 v2), (Cmp lc1 lc2))

colocarLacaios1 :: (Jogador, Jogador, Campo) -> (Jogador, Jogador, Campo) --os lacaios que estao na lista das cartas a serem utilizadas serao postas no campo do
--jogador 1. Assim que as cartas sao postas, sao removidas da mao do jogador, respeitando a mana (anteriormente calculada em pickCards)
colocarLacaios1 ((Jg nome1 cs1 m1 v1), (Jg nome2 cs2 m2 v2), cmp) = ((Jg nome1 (removeAll (lacaios cs1) cs1) (manaDropLacaio (lacaios cs1) m1) v1), (Jg nome2 cs2 m2 v2), (popularCampo1 (lacaioParaCampo cs1) (cmp) ))

colocarLacaios2 :: (Jogador, Jogador, Campo) -> (Jogador, Jogador, Campo) --os lacaios que estao na lista das cartas a serem utilizadas serao postas no campo do
--jogador 2. Assim que as cartas sao postas, sao removidas da mao do jogador, respeitando a mana (anteriormente calculada em pickCards)
colocarLacaios2 ((Jg nome1 cs1 m1 v1), (Jg nome2 cs2 m2 v2), cmp) = ((Jg nome1 cs1 m1 v1), (Jg nome2 (removeAll (lacaios cs2) cs2) (manaDropLacaio (lacaios cs2) m2) v2), (popularCampo2 (lacaioParaCampo cs2) (cmp) ))

ataqueFeitico1 :: (Jogador, Jogador, Campo) -> (Jogador, Jogador, Campo) --os feiticos que estao na lista das cartas a serem utilizadas serao do
--jogador 1. Assim que os feiticos sao usados, sao removidos da mao do jogador, respeitando a mana (anteriormente calculada em pickCards)
ataqueFeitico1 ((Jg nome1 cs1 m1 v1), (Jg nome2 cs2 m2 v2), (Cmp lc1 lc2)) = ((Jg nome1 (removeAll (feiticos cs1) cs1) (manaDropFeitico (feiticos cs1) m1) v1), (Jg nome2 cs2 m2 (danoFeitico (feiticos cs1) v2)), (Cmp lc1 lc2))

ataqueFeitico2 :: (Jogador, Jogador, Campo) -> (Jogador, Jogador, Campo) --os feiticos que estao na lista das cartas a serem utilizadas serao do
--jogador 2. Assim que os feiticos sao usados, sao removidos da mao do jogador, respeitando a mana (anteriormente calculada em pickCards)
ataqueFeitico2 ((Jg nome1 cs1 m1 v1), (Jg nome2 cs2 m2 v2), (Cmp lc1 lc2)) = ((Jg nome1 cs1 m1 (danoFeitico (feiticos cs2) v1)), (Jg nome2 (removeAll (feiticos cs2) cs2) (manaDropFeitico (feiticos cs2) m2) v2), (Cmp lc1 lc2))

turno1 :: (Jogador, Jogador, Campo) -> (Jogador, Jogador, Campo) --Jogador 1 checa se ha lacaios em campo. se nao houver nenhum, coloca e usa feiticos. Senao, 
--ataca com os lacaios ja em campo e tenta utilizar o maximo de cartas possivel (tanto colocar mais lacaios em campo ou feiticos) conforme a regra
turno1 ((Jg nome1 cs1 m1 v1), (Jg nome2 cs2 m2 v2), (Cmp lc1 lc2)) = if (lc1 /= []) then (ataqueFeitico1 . colocarLacaios1 $ ((Jg nome1 (pickCards m1 $ cs1) m1 v1), (Jg nome2 cs2 m2 v2), (Cmp lc1 lc2))) else (ataqueFeitico1 . colocarLacaios1 . ataqueLacaios1 $ ((Jg nome1 cs1 m1 v1), (Jg nome2 cs2 m2 v2), (Cmp lc1 lc2)))

turno2 :: (Jogador, Jogador, Campo) -> (Jogador, Jogador, Campo) --Jogador 2 checa se ha lacaios em campo. se nao houver nenhum, coloca e usa feiticos. Senao, 
--ataca com os lacaios ja em campo e tenta utilizar o maximo de cartas possivel (tanto colocar mais lacaios em campo ou feiticos) conforme a regra
turno2 ((Jg nome1 cs1 m1 v1), (Jg nome2 cs2 m2 v2), (Cmp lc1 lc2)) = if (lc2 /= []) then (ataqueFeitico2 . colocarLacaios2 $ ((Jg nome1 cs1 m1 v1), (Jg nome2 (pickCards m2 $ cs2) m2 v2), (Cmp lc1 lc2))) else (ataqueFeitico2 . colocarLacaios2 . ataqueLacaios2 $ ((Jg nome1 cs1 m1 v1), (Jg nome2 cs2 m2 v2), (Cmp lc1 lc2)))

turn :: (Jogador, Jogador, Campo) -> (Jogador, Jogador, Campo) --realiza um turno do jogador 1 e depois do jogador 2
turn t = turno2 . turno1 $ t