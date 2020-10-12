module UnBCare where

import ModeloDados

{-

data Cuidado = Comprar Medicamento Quantidade |
               Medicar Medicamento
               deriving (Eq, Ord)


██╗░░░██╗███╗░░██╗██████╗░  ░█████╗░░█████╗░██████╗░██████╗
██║░░░██║████╗░██║██╔══██╗  ██╔══██╗██╔══██╗██╔══██╗██╔════╝
██║░░░██║██╔██╗██║██████╦╝  ██║░░╚═╝███████║██████╔╝█████╗░░
██║░░░██║██║╚████║██╔══██╗  ██║░░██╗██╔══██║██╔══██╗██╔══╝░░
╚██████╔╝██║░╚███║██████╦╝  ╚█████╔╝██║░░██║██║░░██║███████╗
░╚═════╝░╚═╝░░╚══╝╚═════╝░  ░╚════╝░╚═╝░░╚═╝╚═╝░░╚═╝╚══════╝

O objetivo desse trabalho é fornecer apoio ao gerenciamento de cuidados a serem prestados a um paciente.
O paciente tem um receituario médico, que indica os medicamentos a serem tomados com seus respectivos horários durante um dia.
Esse receituário é organizado em um plano de medicamentos que estabelece, por horário, quais são os remédios a serem
tomados. Cada medicamento tem um nome e uma quantidade de comprimidos que deve ser ministrada.
Um cuidador de plantão é responsável por ministrar os cuidados ao paciente, seja ministrar medicamento, seja comprar medicamento.
Eventualmente, o cuidador precisará comprar medicamentos para cumprir o plano.
O modelo de dados do problema (definições de tipo) está disponível no arquivo ModeloDados.hs
Defina funções que simulem o comportamento descrito acima e que estejam de acordo com o referido
modelo de dados.

-}

{-

   QUESTÃO 1, VALOR: 1,0 ponto

Defina a função "comprarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento, uma quantidade e um
estoque inicial de medicamentos, retorne um novo estoque de medicamentos contendo o medicamento adicionado da referida
quantidade. Se o medicamento já existir na lista de medicamentos, então a sua quantidade deve ser atualizada no novo estoque.
Caso o remédio ainda não exista no estoque, o novo estoque a ser retornado deve ter o remédio e sua quantidade como cabeça.

-}

atualizaQuantidade :: String -> Int -> [(String, Int)] -> [(String, Int)]
atualizaQuantidade medicamento quantidade ((n, m) : tail)
  | medicamento == n = (n, m + quantidade) : tail
  | otherwise = (n, m) : atualizaQuantidade medicamento quantidade tail

atualizaQuantidade2 :: String -> [(String, Int)] -> [(String, Int)]
atualizaQuantidade2 medicamento ((n, m) : tail)
  | medicamento == n = (n, m - 1) : tail
  | otherwise = (n, m) : atualizaQuantidade2 medicamento tail

mostraQuantidade :: String -> [(String, Int)] -> Int
mostraQuantidade medicamento ((n, m) : tail)
  | medicamento == n = m
  | otherwise = mostraQuantidade medicamento tail

primeiraColuna :: [(a, b)] -> [a]
primeiraColuna lst = [fst x | x <- lst]

segundaColuna :: [(a, b)] -> [b]
segundaColuna lst = [snd x | x <- lst]

primeiraColuna2 :: [(a, b)] -> [a]
primeiraColuna2 lst = foldr (\x acc -> (fst x) : acc) [] lst

comprarMedicamento :: Medicamento -> Quantidade -> EstoqueMedicamentos -> EstoqueMedicamentos
comprarMedicamento medicamento quantidade [] = (medicamento, quantidade) : []
comprarMedicamento medicamento quantidade estoquemedicamentos
  | elem medicamento (primeiraColuna estoquemedicamentos) = atualizaQuantidade medicamento quantidade estoquemedicamentos
  | otherwise = (medicamento, quantidade) : estoquemedicamentos

{-
   QUESTÃO 2, VALOR: 1,0 ponto

Defina a função "tomarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de medicamentos,
retorna um novo estoque de medicamentos, resultante de 1 comprimido do medicamento ser ministrado ao paciente.
Se o medicamento não existir no estoque, Nothing deve ser retornado. Caso contrário, deve se retornar Just v,
onde v é o novo estoque.

-}

tomarMedicamento :: Medicamento -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
tomarMedicamento _ [] = Nothing
tomarMedicamento medicamento estoquemedicamentos
  | elem (medicamento, 0) (estoquemedicamentos) = Nothing
  | elem medicamento (primeiraColuna estoquemedicamentos) = Just (atualizaQuantidade2 medicamento estoquemedicamentos)
  | otherwise = Nothing

{-
   QUESTÃO 3  VALOR: 1,0 ponto

Defina a função "consultarMedicamento", cujo tipo é dado abaixo e que, a partir de um medicamento e de um estoque de
medicamentos, retorne a quantidade desse medicamento no estoque.
Se o medicamento não existir, retorne 0.

-}

consultarMedicamento :: Medicamento -> EstoqueMedicamentos -> Quantidade
consultarMedicamento _ [] = 0
consultarMedicamento medicamento estoquemedicamentos
  | notElem medicamento (primeiraColuna estoquemedicamentos) = 0
  | otherwise = mostraQuantidade medicamento estoquemedicamentos

{-
   QUESTÃO 4  VALOR: 1,0 ponto

  Defina a função "demandaMedicamentos", cujo tipo é dado abaixo e que computa a demanda de todos os medicamentos
  por um dia a partir do receituario. O retorno é do tipo EstoqueMedicamentos e deve ser ordenado lexicograficamente
  pelo nome do medicamento.

  Dica: Observe que o receituario lista cada remédio e os horários em que ele deve ser tomado no dia.
  Assim, a demanda de cada remédio já está latente no receituario, bastando contar a quantidade de vezes que cada remédio
  é tomado.

-}
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x : xs) = quickSort [e | e <- xs, e < x] ++ [x] ++ quickSort [e | e <- xs, e >= x]

calculaDemanda :: Receituario -> [(String, Int)]
calculaDemanda [] = []
calculaDemanda ((x, xs) : tail)
  | x /= [] = (x, length xs) : calculaDemanda tail

demandaMedicamentos :: Receituario -> EstoqueMedicamentos
demandaMedicamentos receituario = quickSort (calculaDemanda receituario)

{-
   QUESTÃO 5  VALOR: 1,0 ponto, sendo 0,5 para cada função.

 Um receituário é válido se, e somente se, todo os medicamentos são distintos e estão ordenados lexicograficamente e,
 para cada medicamento, seus horários também estão ordenados e são distintos.

 Inversamente, um plano de medicamentos é válido se, e somente se, todos seus horários também estão ordenados e são distintos,
 e para cada horário, os medicamentos são distintos e são ordenados lexicograficamente.

 Defina as funções "receituarioValido" e "planoValido" que verifiquem as propriedades acima e cujos tipos são dados abaixo:

 -}

nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x : xs) = x : nub (filter (\y -> x /= y) xs)

temDuplicados :: (Ord a) => [a] -> Bool
temDuplicados xs = length (nub xs) /= length xs

verificaOrdMed :: (Ord a) => [(a, [b])] -> Bool
verificaOrdMed receituario
  | primeiraColuna receituario == quickSort (primeiraColuna receituario) = True
  | otherwise = False

verificaOrdHorario :: (Ord b) => [(a, [b])] -> Bool
verificaOrdHorario [] = True
verificaOrdHorario ((_, xs) : tail)
  | xs == quickSort xs = verificaOrdHorario tail
  | otherwise = False

verificaDupHorario :: (Ord b) => [(a, [b])] -> Bool
verificaDupHorario [] = False
verificaDupHorario ((_, xs) : tail)
  | not (temDuplicados xs) = verificaDupHorario tail
  | otherwise = True

verificaDupMed :: (Ord a) => [(a, [b])] -> Bool
verificaDupMed receituario = temDuplicados (primeiraColuna receituario)

receituarioValido :: Receituario -> Bool
receituarioValido [] = True
receituarioValido receituario
  | sequence [verificaOrdMed, verificaOrdHorario, verificaDupMed, verificaDupHorario] receituario == [True, True, False, False] = True
  | otherwise = False

planoValido :: PlanoMedicamento -> Bool
planoValido [] = True
planoValido planomedico
  | sequence [verificaOrdMed, verificaOrdHorario, verificaDupMed, verificaDupHorario] planomedico == [True, True, False, False] = True
  | otherwise = False

{-

   QUESTÃO 6  VALOR: 1,0 ponto,

 Um plantão é válido se, e somente se, todas as seguintes condições são satisfeitas:

 1. Os horários da lista são distintos e estão em ordem crescente;
 2. Não há, em um mesmo horário, ocorrência de compra e medicagem de um mesmo medicamento (e.g. `[Comprar m1 x, Medicar m1]`);
 3. Para cada horário, as ocorrências de Medicar estão ordenadas lexicograficamente.

 Defina a função "plantaoValido" que verifica as propriedades acima e cujo tipo é dado abaixo:

asdteste :: [(Horario,[Cuidado])] -> Bool
asdteste [] = True
asdteste ((_,c:cs):tail)

lTeste :: [Cuidado] -> [Medicamento]
lTeste a = (filter (\x -> case x of (Comprar _ _) -> True; (Medicar _) -> False) a) >>= (\(Comprar m _) -> [m] )
-}


pMedicar :: [Cuidado] -> [Medicamento]
pMedicar [] = []
pMedicar ((Comprar _ _):tail) = pMedicar tail
pMedicar ((Medicar m):tail) = m : pMedicar tail

pComprar :: [Cuidado] -> [Medicamento]
pComprar [] = []
pComprar ((Medicar _):tail) = pComprar tail
pComprar ((Comprar m _):tail) = m : pComprar tail

temIntersect :: Eq a => [a] -> [a] -> Bool
temIntersect [] _ = False
temIntersect (x:xs) l 
  | elem x l = True
  | otherwise = temIntersect xs l

verifCompraMinistra :: Plantao -> Bool
verifCompraMinistra [] = False
verifCompraMinistra ((_,c):tail)
  | temIntersect (pMedicar c) (pComprar c) = True
  | otherwise = verifCompraMinistra tail

verifOrdMedicar :: Plantao -> Bool
verifOrdMedicar [] = True
verifOrdMedicar ((_,c):tail)   
  | pMedicar c == quickSort (pMedicar c) = verifOrdMedicar tail
  | otherwise = False

 
verificaOrdHorario2 :: (Ord a) => [(a, [b])] -> Bool
verificaOrdHorario2 plantao
  | primeiraColuna plantao == quickSort (primeiraColuna plantao) = True
  | otherwise = False

verificaDupHorario2 :: (Ord a) => [(a, [b])] -> Bool
verificaDupHorario2 plantao = temDuplicados (primeiraColuna plantao)

plantaoValido :: Plantao -> Bool
plantaoValido [] = True
plantaoValido plantao
  | sequence [verificaOrdHorario2, verificaDupHorario2, verifOrdMedicar, verifCompraMinistra] plantao == [True, False, True, False] = True
  | otherwise = False



{-
   QUESTÃO 7  VALOR: 1,0 ponto

  Defina a função "geraPlanoReceituario", cujo tipo é dado abaixo e que, a partir de um receituario válido,
  retorne um plano de medicamento válido.

  Dica: enquanto o receituário lista os horários que cada remédio deve ser tomado, o plano de medicamentos  é uma
  disposição ordenada por horário de todos os remédios que devem ser tomados pelo paciente em um certo horário. testando auto commit

testeMap :: Receituario -> Horario -> Medicamento -> [(Horario, [Medicamento])]
testeMap r h m = map (\x -> if (fst x) == h then (h, quickSort (m : (snd x))) else x) (teste2 r)

-}

remDup :: Eq a => [a] -> [a]
remDup =
  foldl
    ( \memo x ->
        if x `elem` memo
          then memo
          else memo ++ [x]
    )
    []

retornaMedHorario :: Horario -> [(Horario, Medicamento)] -> [Medicamento]
retornaMedHorario _ [] = []
retornaMedHorario n ((h,m):tail)
   | h == n = m : (retornaMedHorario n tail)
   | otherwise = retornaMedHorario n tail

listaHorarios :: Receituario -> [Horario]
listaHorarios r = remDup (quickSort (concat (segundaColuna r)))

teste1 :: (Medicamento, [Horario]) -> [(Horario, Medicamento)]
teste1 (_, []) = []
teste1 (m, h : tail) = (h, m) : (teste1 (m, tail))

teste2 :: Receituario -> [(Horario, Medicamento)]
teste2 [] = []
teste2 (h : tail) = quickSort (((teste1 h)) ++ (teste2 tail)) 

percorreLista :: [Horario] -> Receituario -> PlanoMedicamento 
percorreLista [] _ = []
percorreLista (h:hs) r = (h,retornaMedHorario h (teste2 r)) : percorreLista hs r

geraPlanoReceituario :: Receituario -> PlanoMedicamento
geraPlanoReceituario r = percorreLista (listaHorarios r) r

{- QUESTÃO 8  VALOR: 1,0 ponto

 Defina a função "geraReceituarioPlano", cujo tipo é dado abaixo e que retorna um receituário válido a partir de um
 plano de medicamentos válido.
 Dica: Existe alguma relação de simetria entre o receituário e o plano de medicamentos? Caso exista, essa simetria permite
 compararmos a função geraReceituarioPlano com a função geraPlanoReceituario ? Em outras palavras, podemos definir
 geraReceituarioPlano com base em geraPlanoReceituario?

-}

retornaHorMedicamento :: Medicamento -> [(Medicamento, Horario)] -> [Horario]
retornaHorMedicamento _ [] = []
retornaHorMedicamento n ((h,m):tail)
   | h == n = m : (retornaHorMedicamento n tail)
   | otherwise = retornaHorMedicamento n tail

listaMeds :: PlanoMedicamento -> [Medicamento]
listaMeds r = remDup (quickSort (concat (segundaColuna r)))

teste11 :: (Horario, [Medicamento]) -> [(Medicamento, Horario)]
teste11 (_, []) = []
teste11 (m, h : tail) = (h, m) : (teste11 (m, tail))

teste22 :: PlanoMedicamento -> [(Medicamento, Horario)]
teste22 [] = []
teste22 (h : tail) = quickSort (((teste11 h)) ++ (teste22 tail)) 

percorreLista2 :: [Medicamento] -> PlanoMedicamento -> Receituario 
percorreLista2 [] _ = []
percorreLista2 (h:hs) p = (h,retornaHorMedicamento h (teste22 p)) : percorreLista2 hs p

geraReceituarioPlano :: PlanoMedicamento -> Receituario
geraReceituarioPlano p = percorreLista2 (listaMeds p) p

{-  QUESTÃO 9 VALOR: 1,0 ponto

Defina a função "executaPlantao", cujo tipo é dado abaixo e que executa um plantão válido a partir de um estoque de medicamentos,
resultando em novo estoque. A execução consiste em desempenhar, sequencialmente, todos os cuidados para cada horário do plantão.
Caso o estoque acabe antes de terminar a execução do plantão, o resultado da função deve ser Nothing. Caso contrário, o resultado
deve ser Just v, onde v é o valor final do estoque de medicamentos

-}

unMaybe :: Monoid p => Maybe p -> p
unMaybe Nothing = mempty
unMaybe (Just v) = v

executaCuidado :: Cuidado -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaCuidado _ [] = Nothing
executaCuidado (Comprar m q) e = Just (comprarMedicamento m q e)
executaCuidado (Medicar m) e = tomarMedicamento m e

executaCuidados :: [Cuidado] -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaCuidados [] [] = Nothing
executaCuidados [] e = Just e
executaCuidados (c:cs) e = executaCuidados cs (unMaybe (executaCuidado c e))

executaPlantao :: Plantao -> EstoqueMedicamentos -> Maybe EstoqueMedicamentos
executaPlantao [] [] = Nothing
executaPlantao [] e = Just e
executaPlantao ((_,c):ps) e = executaPlantao ps (unMaybe (executaCuidados c e))

{-
QUESTÃO 10 VALOR: 1,0 ponto

Defina uma função "satisfaz", cujo tipo é dado abaixo e que verifica se um plantão válido satisfaz um plano
de medicamento válido para um certo estoque, ou seja, a função "satisfaz" deve verificar se a execução do plantão
implica terminar com estoque diferente de Nothing e administrar os medicamentos prescritos no plano.
Dica: fazer correspondencia entre os remédios previstos no plano e os ministrados pela execução do plantão.
Note que alguns cuidados podem ser comprar medicamento e que eles podem ocorrer sozinhos em certo horário ou
juntamente com ministrar medicamento.

-}

tipoCuidado :: [Cuidado] -> [String]
tipoCuidado [] = []
tipoCuidado ((Comprar _ _):tail) = "Comprar" : tipoCuidado tail
tipoCuidado ((Medicar _):tail) = "Medicar" : tipoCuidado tail

filtraPlantao :: Plantao -> Plantao
filtraPlantao [] = []
filtraPlantao (p:ps)
  | notElem "Medicar" (tipoCuidado(snd p)) = filtraPlantao ps
  | otherwise = p : filtraPlantao ps

verifPlantaoPlano :: Plantao -> PlanoMedicamento -> Bool
verifPlantaoPlano [] [] = True
verifPlantaoPlano ((h,c):ps) ((h2,m):pms)
  | h /= h2 = False
  | (pMedicar c) /= m = False
  | otherwise = verifPlantaoPlano ps pms

satisfaz :: Plantao -> PlanoMedicamento -> EstoqueMedicamentos -> Bool
satisfaz p pm e = ((executaPlantao (filtraPlantao p) e) /= Nothing) && verifPlantaoPlano (filtraPlantao p) pm

{-

QUESTÃO 11 (EXTRA) VALOR: 1,0 ponto

 Defina a função "plantaoCorreto", cujo tipo é dado abaixo 
 e que gera um plantão válido que satisfaz um plano de
 medicamentos válido e um estoque de medicamentos.
 Dica: a execução do plantão deve atender ao plano 
 de medicamentos e ao estoque.

-}

geraCompras :: [(Medicamento, Quantidade)] -> [Cuidado]
geraCompras [] = []
geraCompras ((m,q):ds) = (Comprar m q) : geraCompras ds

geraCuidados :: [Medicamento] -> [Cuidado]
geraCuidados [] = []
geraCuidados (m:ds) = (Medicar m) : geraCuidados ds

--(h-1,(geraCompras (demandaMedicamentos (geraReceituarioPlano plano))))

geraPlantao :: PlanoMedicamento -> EstoqueMedicamentos -> Plantao
geraPlantao  [] _ = []
geraPlantao ((h,m):tail) _ = (h,geraCuidados m) : (geraPlantao tail [])

geraPlantaoAux :: PlanoMedicamento -> (Horario, [Cuidado])
geraPlantaoAux ((h,m):tail) = (h-1,(geraCompras (demandaMedicamentos (geraReceituarioPlano ((h,m):tail)))))

plantaoCorreto :: PlanoMedicamento -> EstoqueMedicamentos -> Plantao
plantaoCorreto  [] _ = []
plantaoCorreto p _ = geraPlantaoAux : 

{- 
consultarMedicamento
demandaMedicamentos
geraPlanoReceituario
geraReceituarioPlano 
plantaoValido
-}