module Exemplos where

import ModeloDados
import UnBCare

-- Declarações

m1 :: Medicamento
m1 = "Lactulona"

m2 :: Medicamento
m2 = "Pantoprazol"

m3 :: Medicamento
m3 = "Patz"

m4 :: Medicamento
m4 = "Quetiapina"

m5 :: Medicamento
m5 = "Mirtazapina"

m6 :: Medicamento
m6 = "Adera"

m7 :: Medicamento
m7 = "Donepezila"

m8 :: Medicamento
m8 = "Xarelto"

m9 :: Medicamento
m9 = "Alprazolam"

estoque1 :: EstoqueMedicamentos
estoque1 = [(m1,10), (m2,5), (m3,0)]

estoque2 :: EstoqueMedicamentos
estoque2 = [(m1,10), (m2,5), (m3,10)]

estoque3 :: EstoqueMedicamentos
estoque3 = [(m1,10), (m2,50), (m3,10), (m4, 20)]

receituario1 :: Receituario
receituario1 = [(m1,[8,17]),(m2,[6]),(m3,[22])]

receituario2 :: Receituario
receituario2 = [(m1,[8,17]),(m2,[6]),(m3,[22]), (m4,[8,22,23])]

plano1 :: PlanoMedicamento
plano1 = [(6,[m2]),(8,[m1]),(17,[m1]),(22,[m3])]

plano2 :: PlanoMedicamento
plano2 = [(6,[m2]),(8,[m1,m4]),(17,[m1]),(22,[m3,m4]), (23,[m4])] :: [(Int, [String])]

plantao1 :: Plantao
plantao1 = [(6,[Medicar m2])
           ,(8,[Medicar m1])
           ,(17,[Medicar m1])
           ,(22,[Medicar m3])]

plantao2 :: Plantao
plantao2 = [(6,[Medicar m2])
           ,(8,[Medicar m1])
           ,(17,[Medicar m1, Comprar m3 30])
           ,(22,[Medicar m3])]

plantaoValido0 :: Plantao
plantaoValido0 = [(6,[Medicar m2, Medicar m8])
           ,(8,[Medicar m9, Medicar m1])
           ,(17,[Medicar m1, Comprar m3 30])
           ,(22,[Medicar m3])]

plantaoInvalido1 :: Plantao
plantaoInvalido1 = [(6,[Medicar m2, Medicar m8])
                   ,(8,[Medicar m9, Medicar m1])
                   ,(22,[Medicar m3])
                   ,(17,[Medicar m1, Comprar m3 30])]

plantaoInvalido2 :: Plantao
plantaoInvalido2 = [(6,[Medicar m2, Medicar m8])
                   ,(8,[Medicar m9, Medicar m1])
                   ,(17,[Medicar m1, Comprar m1 30])
                   ,(22,[Medicar m3])]

plantaoInvalido3 :: Plantao
plantaoInvalido3 = [(6,[Medicar m8, Medicar m2])
                   ,(8,[Medicar m9, Medicar m1])
                   ,(17,[Medicar m1, Comprar m1 30])
                   ,(22,[Medicar m3])]

plantaoInvalido4 :: Plantao
plantaoInvalido4 = [(6, [Medicar m2])
                   ,(8, [Comprar m1 20, Medicar m1, Medicar m4])
                   ,(17, [Medicar m1])
                   ,(22, [Medicar m3, Medicar m4])
                   ,(23, [Medicar m4])
                   ]

-- testando comprarMedicamento
casoTeste1 = comprarMedicamento m3 30 estoque1 == [(m1,10), (m2,5), (m3,30)]
casoTeste2 = comprarMedicamento m6 20 estoque1 == [(m6,20),(m1,10),(m2,5),(m3,0)]
casoTeste3 = comprarMedicamento m2 2 estoque1 == [(m1,10), (m2,7), (m3,0)]
casoTeste4 = comprarMedicamento m8 20 [] == [(m8,20)]
conjuntoCasosTeste1 = and [casoTeste1,casoTeste2,casoTeste3,casoTeste4]

--testando tomarMedicamento
casoTeste5 = tomarMedicamento m1 estoque1 == Just [(m1,9), (m2,5), (m3,0)]
casoTeste6 = tomarMedicamento m3 estoque1 == Nothing
conjuntoCasosTeste2 = and [casoTeste5,casoTeste6]

--testando consultarMedicamento
casoTeste7 = consultarMedicamento m2 estoque1 == 5
casoTeste8 = consultarMedicamento "Aas" estoque1 == 0
conjuntoCasosTeste3 = and [casoTeste5,casoTeste6]

-- testando demandaMedicamentos
casoTeste9 = demandaMedicamentos receituario1 == [(m1,2),(m2,1),(m3,1)]
conjuntoCasosTeste4 = and [casoTeste9]

--testando geraPlanoReceituario
casoTeste10 = geraPlanoReceituario receituario1 == [(6,[m2]),(8,[m1]),(17,[m1]),(22,[m3])]
casoTeste11 = geraPlanoReceituario receituario2 == [(6,[m2]),(8,[m1,m4]),(17,[m1]),(22,[m3,m4]), (23,[m4])]
conjuntoCasosTeste5 = and [casoTeste10,casoTeste11]

-- testando geradores de plano e receituário
casoTeste12 = geraReceituarioPlano (geraPlanoReceituario receituario1) == receituario1
casoTeste13 = geraReceituarioPlano (geraPlanoReceituario receituario2) == receituario2
conjuntoCasosTeste6 = and [casoTeste12,casoTeste13]

-- testando executaPlantao
casoTeste14 = executaPlantao plantao1 estoque1 == Nothing
casoTeste15 = executaPlantao plantao1 estoque2 == Just [(m1,8), (m2,4), (m3,9)]
casoTeste16 = executaPlantao plantao2 estoque1 == Just [(m1,8), (m2,4), (m3,29)]

conjuntoCasosTeste7 = and [casoTeste14,casoTeste15,casoTeste16]

-- testando satisfaz
casoTeste17 = not (satisfaz plantao1 plano1 estoque1)
casoTeste18 = satisfaz plantao1 plano1 estoque2
casoTeste19 = satisfaz plantao2 plano1 estoque1
conjuntoCasosTeste8 = and [casoTeste17,casoTeste18,casoTeste19]


-- testando plantaoCorreto
casoTeste21 = satisfaz plantao plano1 estoque1
  where plantao = plantaoCorreto plano1 estoque1
conjuntoCasosTeste9 = and [casoTeste21]

-- resultado global dos testes
resultadoGlobalTestes = and [conjuntoCasosTeste1,conjuntoCasosTeste2,conjuntoCasosTeste3,
                             conjuntoCasosTeste4,conjuntoCasosTeste5,conjuntoCasosTeste6,
                             conjuntoCasosTeste7,conjuntoCasosTeste8,conjuntoCasosTeste9,
                             conjuntoCasosTestePlantaoValido]


-- testando plantaoValido
conjuntoCasosTestePlantaoValido =
  and [ plantaoValido plantao1 == True
      , plantaoValido plantao2 == True
      , plantaoValido plantaoValido0 == True
      , plantaoValido plantaoInvalido1 == False
      , plantaoValido plantaoInvalido2 == False
      , plantaoValido plantaoInvalido3 == False
      ]




