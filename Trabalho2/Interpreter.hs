{- IDENTIFICAÇÃO DO TRABALHO (de cada membro do grupo):

 Nome completo: João Victor Bohrer Munhoz               Matrícula: 16/0071101
 Nome completo: Matheus Gabriel da Silva Rodrigues      Matrícula: 18/0025031
 Nome completo: Vinícius de Melo Santos                 Matrícula: 17/0157849
 
-}

module Interpreter where

import AbsLI
import Prelude hiding (lookup)
import PrintLI

-- ***Teste 1***
exemploReturn1 = Prog [Fun Tint (Ident "main") [] 
                    [SDec (Dec Tint (Ident "c")), 
                    SDec (Dec Tint (Ident "soma")),
                    SAss (Ident "c") (EInt 10),
                    SAss (Ident "soma") (EInt 0),
                    SRepeat (SBlock [SAss (Ident "soma") (EAdd (EVar (Ident "soma")) (EVar (Ident "c"))),
                                     SAss (Ident "c") (ESub (EVar (Ident "c")) (EInt 1))])
                        (EVar (Ident "c")),
                    SReturn (EVar (Ident "soma"))]]

{-  
 exemploReturn1 acima representa o seguinte programa 
 fonte da LI2Tipada evoluida com "repeat until"

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
>ghci Interpreter
> executeP exemploReturn1
([[("c",0),("soma",55),("return",55)]],[("main",int<-())])

-}


-- ***Teste 2***
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
> ghci Interpreter
> executeP  exemploReturn2
([[("x",7),("r",5040),("return",5040)]],[("main",int<-()),("fatorial",int<-(int n))])

-}

exemploReturn3 = Prog [Fun Tint (Ident "main") [] 
                    [SDec (Dec Tint (Ident "c")), 
                    SDec (Dec Tint (Ident "soma")),
                    SAss (Ident "c") (EInt 0),
                    SAss (Ident "soma") (EInt 0),
                    SWhile (EVar (Ident "c"))
                      (SBlock [SAss (Ident "soma") (EAdd (EVar (Ident "soma")) (EInt 10)),
                                     SAss (Ident "c") (ESub (EVar (Ident "c")) (EInt 1))]),                       
                    SReturn (EVar (Ident "soma"))]]

{-
int main () {
  int c;
  int soma;
  c = 0;
  soma = 0;
  do {
      soma = soma + 10;
      c = c - 1;
    }
  while (c) ;
 return soma;
 }
([[("c",0),("soma",0),("return",0)]],[("main",int<-())])
-}

exemploReturn4 = Prog [Fun Tint (Ident "main") [] 
                    [SDec (Dec Tint (Ident "c")), 
                    SDec (Dec Tint (Ident "soma")),
                    SAss (Ident "c") (EInt 0),
                    SAss (Ident "soma") (EInt 0),
                    SRepeat (SBlock [SAss (Ident "soma") (EAdd (EVar (Ident "soma")) (EInt 10)),
                    -- SRepeat (SBlock [SAss (Ident "soma") (EAdd (EVar (Ident "soma")) (EInt 1)),
                                     SAss (Ident "c") (ESub (EVar (Ident "c")) (EInt 1))])
                        (EVar (Ident "c")),
                    SReturn (EVar (Ident "soma"))]]

{-
int main () {
  int c;
  int soma;
  c = 0;
  soma = 0;
  repeat {
      soma = soma + 10;
      c = c - 1;
    }
  until (c) ;
 return soma;
 }
([[("c",-1),("soma",10),("return",10)]],[("main",int<-())])
-}


executeP :: Program -> Environment

executeP (Prog fs) =  execute (updatecF ([],[]) fs) ( SBlock (stmMain fs))
    where stmMain ((Fun _ (Ident "main") decls stms):xs) = stms
          stmMain ( _ :xs) = stmMain xs                                            
   


execute :: Environment -> Stm -> Environment
execute environment x = case x of
   SDec (Dec tp id) -> updateShallowValue environment id (initVal tp)
   SAss id exp -> updateDeepValue environment id (eval environment exp)
   SBlock [] -> environment
   SBlock (sb@(SBlock bls):stms) -> execute (pop (execute (push environment) sb)) (SBlock stms)
   SBlock (s:stms) -> execute (execute environment s) (SBlock stms) 
   SWhile exp stm -> if ( i (eval environment exp) /= 0) 
                      then execute (execute environment stm) (SWhile exp stm)
                      else environment
   SReturn exp ->  updateShallowValue environment  (Ident "return")  (eval environment exp)
   SIf exp stmT stmE -> if ( i (eval environment exp) /= 0) 
                          then execute environment stmT
                          else execute environment stmE
   SRepeat stm exp -> if (i (eval (execute environment stm) exp) /= 0)
                        then execute (execute environment stm) (SRepeat stm exp)
                        else execute environment stm
  {- SRepeat stm exp -> if ( i (eval environment exp) > 0) 
                      then execute (execute environment stm) (SRepeat stm exp)
                      else environment                       
   SRepeat stm exp -> let a = execute (execute environment stm) stm
                      in if ( i (eval a exp) /= 0 ) 
                        then execute (execute environment stm) (SRepeat stm exp)
                        else a-} 

eval :: Environment -> Exp -> Valor
eval environment x = case x of
    ECon exp0 exp  -> ValorStr ( s (eval environment exp0) ++  s (eval environment exp) )
    EAdd exp0 exp  -> ValorInt ( i (eval environment exp0)  +  i (eval environment exp))
    ESub exp0 exp  -> ValorInt ( i (eval environment exp0)  -  i (eval environment exp)) 
    EMul exp0 exp  -> ValorInt ( i (eval environment exp0)  *  i (eval environment exp))
    EDiv exp0 exp  -> ValorInt ( i (eval environment exp0) `div` i (eval environment exp)) 
    EOr  exp0 exp  -> ValorBool ( b (eval environment exp0)  || b (eval environment exp))
    EAnd exp0 exp  -> ValorBool ( b (eval environment exp0)  && b (eval environment exp))
    ENot exp       -> ValorBool ( not (b (eval environment exp)))
    EStr str       -> ValorStr str
    ETrue          -> ValorBool True
    EFalse         -> ValorBool False
    EInt n         -> ValorInt n
    EVar id        -> lookupDeepValue environment  id
    Call id lexp   -> lookupShallowValue (execute ( [paramBindings],(snd environment)) (SBlock stms)) 
                                         (Ident "return")
                          where ValorFun (Fun _ _ decls stms) = lookupShallowFunction environment id
                                paramBindings = zip (map (\(Dec _ id) -> id) decls) 
                                                    (map (eval environment) lexp)

data Valor = ValorInt {
               i :: Integer         
             }
            | 
             ValorFun {
               f :: Function
             }   
            | 
             ValorStr {
               s :: String
             } 
            | ValorBool {
               b :: Bool
             }


initVal :: Type -> Valor
initVal Tbool = ValorBool False
initVal Tint  = ValorInt 0
initVal TStr  = ValorStr ""


instance Show Ident where
  show (Ident s) = printTree s


instance Show Valor where
  show (ValorBool b) = show b
  show (ValorInt i) = show i
  show (ValorStr s) = s
  show (ValorFun (Fun tp _ decls _)) = printTree tp ++ "<-" ++ "(" ++ printTree decls ++ ")" 
--(\(Ident x) -> x) nf


data R a = OK a | Erro String                                   
         deriving (Eq, Ord, Show, Read)

type Environment = ([RContext],RContext)
type RContext = [(Ident,Valor)]

pushB :: RContext -> Environment -> Environment
pushB typeBindings (sc,fnCtx) = (typeBindings:sc,fnCtx) 

push :: Environment -> Environment
push (sc,fnCtx) = ([]:sc,fnCtx)
 
pop :: Environment -> Environment
pop ((s:scs),fnCtx) = (scs,fnCtx)

lookupDeepValue :: Environment -> Ident -> Valor
--lookupDeepType ([],fnCtx) id = Erro (printTree id ++ " nao esta no contexto. ")
lookupDeepValue ((s:scs),fnCtx) id =  let r = lookupShallow s id in
                                         case r of
                                            OK val -> val
                                            Erro _ -> lookupDeepValue (scs,fnCtx) id

lookupShallowValue :: Environment -> Ident -> Valor   
lookupShallowValue  ((s:sc),_) id =  (\(OK val) -> val) (lookupShallow s id)
                                      
lookupShallowFunction :: Environment -> Ident -> Valor
lookupShallowFunction (_,fnCtx) id = (\(OK val) -> val) (lookupShallow fnCtx id)

lookupShallow :: RContext -> Ident -> R Valor
lookupShallow [] s = Erro (show s ++ " nao esta no contexto. ")
lookupShallow ((i,v):cs) s
   | i == s =  OK v
   | otherwise = lookupShallow cs s

updateShallowValue :: Environment -> Ident -> Valor -> Environment
updateShallowValue ([],fnCtx) id tp = ([[(id,tp)]],fnCtx)
updateShallowValue ((s:sc),fnCtx) id tp = ( (updateShallow s id tp):sc , fnCtx)   

updateDeepValue :: Environment -> Ident -> Valor -> Environment
updateDeepValue ([],fnCtx) id tp = ([[(id,tp)]],fnCtx)
updateDeepValue ((s:sc),fnCtx) id tp = let r = lookupShallow s id in 
                                           case r of
                                               OK _ -> ( (updateShallow s id tp):sc , fnCtx)
                                               Erro _ -> pushB s (updateDeepValue (sc,fnCtx) id tp)    
                                             
--note that, differently from the typechecker, updating an existing value is possible
updateShallow :: RContext -> Ident -> Valor -> RContext
updateShallow [] s nv = [(s,nv)]
updateShallow ((i,v):cs) s nv
        | i == s = (i,nv):cs
        | otherwise = (i,v) : updateShallow cs s nv
 
updatecF :: Environment -> [Function] -> Environment
updatecF e [] = e
updatecF (sc,c) (f@(Fun tp id params stms):fs) = updatecF (sc, updateShallow c id (ValorFun f)) fs
                                                     

