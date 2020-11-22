{- IDENTIFICAÇÃO DO TRABALHO (de cada membro do grupo):

 Nome completo: João Victor Bohrer Munhoz               Matrícula: 16/0071101
 Nome completo: Matheus Gabriel da Silva Rodrigues      Matrícula: 18/0025031
 Nome completo: Vinícius de Melo Santos                 Matrícula: 17/0157849
 
-}

module Typechecer where

import AbsLI
import Prelude hiding (lookup)
import PrintLI

-- *** TESTE 1 ***
exemploReturn1 = Prog [Fun Tint (Ident "main") [] 
                    [SDec (Dec Tint (Ident "c")), 
                    SDec (Dec Tint (Ident "soma")),
                    SAss (Ident "c") (EInt 10),
                    SAss (Ident "soma") (EInt 0),
                    SRepeat (SBlock [SAss (Ident "soma") (EAdd (EVar (Ident "soma")) (EVar (Ident "c"))),
                                     SAss (Ident "c") (ESub (EVar (Ident "c")) (EInt 1))])
                        (EVar (Ident "c")),
                    SReturn (EVar (Ident "soma"))]]               

{- exemploReturn1 acima representa o seguinte programa fonte da LI2Tipada evoluida com "repeat until":

int main () {
  int c;
  int soma;
  c = 10;
  soma = 0;
  repeat {
      soma = soma + c;
      c = c - 1;
	}
  until (c) ;
 return soma;
}

Numa sessão com o GHCI, o seguinte resultado é esperado:
> ghci Typechecer
> typeCheckP exemploReturn1
[OK ([[("soma",int),("c",int)]],[("main",int<-())])]

-}

-- *** TESTE 2 ***
exemploReturn2 = Prog [Fun Tint (Ident "main") []
                        [SDec (Dec Tint (Ident "x")),
                         SAss (Ident "x") (EInt 7),
                         SDec (Dec Tint (Ident "r")),
                         SAss (Ident "r") (Call (Ident "fatorial") [EVar (Ident "x")]),
                         SReturn (EVar (Ident "r"))],
                       Fun Tint (Ident "fatorial") [Dec Tint (Ident "n")] 
                        [SDec (Dec Tint (Ident "p")),
                         SAss (Ident "p") (EInt 1),
                         SIf (EVar (Ident "n")) 
                          (SRepeat (SBlock [SAss (Ident "p") (EMul (EVar (Ident "n")) (EVar (Ident "p"))),
                                            SAss (Ident "n") (ESub (EVar (Ident "n")) (EInt 1))]) 
                                   (EVar (Ident "n"))) 
                          (SAss (Ident "p") (EInt 1)),
                         SReturn (EVar (Ident "p"))]]

{-

int main () {
 int x;
 x = 7;
 int r ;
 r = fatorial (x);
 return r;
}

int fatorial (int n) {
 int p;
 p = 1;
 if (n)
   then 
     repeat {
             p = n * p;
             n = n - 1;
		    }
     until ( n ) ;
   else p = 1;
 return p;
}

-}                                                                  

{- 
exemploReturn2 acima representa o seguinte programa fonte da LI2Tipada evoluida com "repeat until"

int main () {
 int x;
 x = 7;
 int r ;
 r = fatorial (x);
 return r;
}

int fatorial (int n) {
 int p;
 p = 1;
 if (n)
   then 
     repeat {
             p = n * p;
             n = n - 1;
		    }
     until ( n ) ;
   else p = 1;
 return p;
}

Numa sessão com o GHCI, o seguinte resultado é esperado:
> ghci Typechecer
> typeCheckP exemploReturn2
[OK ([[("r",int),("x",int)]],[("fatorial",int<-(int n)),("main",int<-())]),OK ([[("p",int),("n",int)]],
[("fatorial",int<-(int n)),("main",int<-())])]
-}


data R a = OK a | Erro String                                   
         deriving (Eq, Ord, Show, Read)



typeCheckP :: Program  -> [R Environment]
typeCheckP (Prog fs) = let environment = updatecF ([],[]) fs in
                          case environment of
                             OK env -> map (typeCheckF env) fs
                             Erro msg -> [Erro msg]

typeCheckF ::  Environment -> Function -> R Environment
typeCheckF environment (Fun tR id decls stms) = if (tR /= Tvoid)
                                              then 
                                                if (stms /= [])
                                                  then let lastCommand = last stms in
                                                         case lastCommand of
                                                            SReturn exp -> let r = tk newEnvironment (SBlock stmsButLast) in
                                                                             case r of 
                                                                                OK env -> tke env exp tR
                                                                                Erro msg -> Erro msg
                                                                           where stmsButLast  = reverse (tail (reverse stms)) 
                                                            _           -> Erro ("Na funcao " ++ show id ++ ", nao ha comando retornando valor do tipo " ++ show tR)
                                                  else Erro ("Nao ha comando retornando valor do tipo da funcao " ++ show tR)
                                              else tk newEnvironment (SBlock stms)
                                            where typeBindings = map (\(Dec tp id) -> (id,tp)) decls
                                                  newEnvironment = pushB typeBindings environment 

tk :: Environment -> Stm -> R Environment
tk environment x = case x of
   cAss@(SAss id exp) -> let r = tinf environment exp in
                            case r of
                               OK x -> let r2 = tke environment (EVar id) x in 
                                          case r2 of 
                                             OK env -> OK env
                                             Erro msg -> Erro (msg ++ " no comando: " ++ printTree cAss)
                               Erro msg -> Erro (msg ++ " no comando: " ++ printTree cAss)
   SBlock [] -> OK environment
   SBlock ( cDec@(SDec (Dec tp id)):stms) -> let r = updateShallowType environment id tp in 
                                                  case r of 
                                                    OK env  -> tk env (SBlock stms)
                                                    Erro msg -> Erro (msg ++ " no comando: " ++ printTree cDec)
   SBlock (sb@(SBlock bls):stms) -> let r = tk (push environment) sb in
                                        case r of
                                           OK _ -> tk environment (SBlock stms)
                                           Erro msg -> Erro (msg ++ " no comando: " ++ printTree sb) 
   SBlock (s:stms) -> let r = tk environment s in
                         case r of 
                            OK _ -> tk environment (SBlock stms)
                            Erro msg -> Erro msg
   cWhile@(SWhile exp stm) ->  let r = tke environment exp Tint in 
                                  case r of 
                                     OK _ -> tk environment stm
                                     Erro msg -> Erro (msg ++ " no comando: " ++ printTree cWhile)
   cIf@(SIf exp stmT stmE) ->  let r = tke environment exp Tint in
                                  case r of
                                     OK _ -> let r2 = tk environment stmT in 
                                                case r2 of
                                                   OK _ ->  tk environment stmE
                                                   Erro msg -> Erro (msg ++ " no comando: " ++ printTree cIf)
                                     Erro msg -> Erro (msg ++ " no comando: " ++ printTree cIf)
   cRepeat@(SRepeat stm exp) ->  let r = tke environment exp Tint in 
                                  case r of 
                                     OK _ -> tk environment stm
                                     Erro msg -> Erro (msg ++ " no comando: " ++ printTree cRepeat)                                  
   

tke :: Environment -> Exp -> Type -> R Environment
tke environment exp tp = let r = tinf environment exp in
                          case r of
                             OK tipo -> if (tipo == tp )
                                           then OK environment
                                           else Erro (" A expressao "++ printTree exp ++ " tem tipo " ++ printTree tipo ++
                                                      " mas o tipo esperado eh " ++ printTree tp)
                             Erro msg -> Erro msg  

combChecks :: Environment -> Exp -> Exp -> Type -> R Type
combChecks environment exp1 exp2 tp = let r = tke environment exp1 tp in
                                       case r of
                                          OK _ -> let r2 = tke environment exp2 tp in
                                                     case r2 of 
                                                         OK _ -> OK tp
                                                         Erro msg -> Erro msg
                                          Erro msg -> Erro msg 


tinf :: Environment -> Exp -> R Type
tinf environment x  =  case x of
    ECon exp0 exp  -> combChecks environment exp0 exp TStr
    EAdd exp0 exp  -> combChecks environment exp0 exp Tint
    ESub exp0 exp  -> combChecks environment exp0 exp Tint
    EMul exp0 exp  -> combChecks environment exp0 exp Tint
    EDiv exp0 exp  -> combChecks environment exp0 exp Tint
    EOr  exp0 exp  -> combChecks environment exp0 exp Tbool
    EAnd exp0 exp  -> combChecks environment exp0 exp Tbool
    ENot exp       -> let r = tke environment exp Tbool in 
                         case r of 
                             OK _ -> OK Tbool
                             Erro msg -> Erro msg
    EStr str       -> OK TStr  
    ETrue          -> OK Tbool 
    EFalse         -> OK Tbool  
    EInt n         -> OK Tint  
    EVar id        -> lookupDeepType environment id
    Call id lexp   -> let r = lookupShallowFunction environment id in 
                          case r of 
                             OK (TFun (Fun tipoRetorno _ decls stms)) -> if (length decls == length lexp)
                                                                         then 
                                                                            if ( isThereError tksArgs /= [])
                                                                              then Erro "chamada de funcao invalida"
                                                                              else OK tipoRetorno
                                                                         else Erro " tamanhos diferentes de lista de argumentos e parametros"
                                                                        where 
                                                                              parameterTypes =  map (\(Dec tp _ )-> tp) decls
                                                                              tksArgs = zipWith (tke environment) lexp parameterTypes
                                                                              isThereError l = filter (==False) (map (\e->(let r2 = e in  
                                                                                                                            case r2 of
                                                                                                                              OK _ -> True
                                                                                                                              Erro _ -> False)) 
                                                                                                                     l)
                                                 
                             Erro msg -> Erro msg


instance Show Ident where
  show (Ident s) = printTree s


instance Show Type where
  show (Tbool) = printTree Tbool
  show (Tint)  = printTree Tint
  show (Tvoid) = printTree Tvoid
  show (TStr)  = printTree TStr
  show (TFun (Fun tp _ decls _)) = printTree tp ++ "<-" ++ "(" ++ printTree decls ++ ")" 

type Environment = ([RContext],RContext)
type RContext = [(Ident,Type)]

pushB :: RContext -> Environment -> Environment
pushB typeBindings (sc,fnCtx) = (typeBindings:sc,fnCtx) 

push :: Environment -> Environment
push (sc,fnCtx) = ([]:sc,fnCtx)
 
pop :: Environment -> Environment
pop ((s:scs),fnCtx) = (scs,fnCtx)

lookupDeepType :: Environment -> Ident -> R Type
lookupDeepType ([],fnCtx) id = Erro (printTree id ++ " nao esta no contexto. ")
lookupDeepType ((s:scs),fnCtx) id =  let r = lookupShallow s id in
                                         case r of
                                            OK tp -> OK tp
                                            Erro _ -> lookupDeepType (scs,fnCtx) id

lookupShallowFunction :: Environment -> Ident -> R Type
lookupShallowFunction (sc,fnCtx) id = lookupShallow fnCtx id

lookupShallow :: RContext -> Ident -> R Type
lookupShallow [] s = Erro (printTree s ++ " nao esta no contexto. ")
lookupShallow ((i,v):cs) s
   | i == s = OK v
   | otherwise = lookupShallow cs s

updateShallowType :: Environment -> Ident -> Type -> R Environment
updateShallowType ([],fnCtx) id tp = OK ([[(id,tp)]],fnCtx)
updateShallowType ((s:sc),fnCtx) id tp = let r = updateShallow s id tp  in 
                                             case r of 
                                                OK cxt -> OK (cxt:sc,fnCtx)
                                                Erro msg -> Erro msg

updateShallow :: RContext -> Ident -> Type -> R RContext
updateShallow context id tp = if (elem id (map fst context))
                                then Erro "tipo ja definido no contexto de tipos"
                                else OK ((id,tp):context)

updatecF :: Environment -> [Function] -> R Environment
updatecF e [] = OK e
updatecF (sc,c) (f@(Fun tp id params stms):fs) = let r = updateShallow c id (TFun f) in
                                                   case r of 
                                                     OK ctx -> updatecF (sc,ctx) fs
                                                     Erro msg -> Erro msg

