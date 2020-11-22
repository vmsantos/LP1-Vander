{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Pretty-printer for PrintLI.
--   Generated by the BNF converter.

module PrintLI where

import qualified AbsLI
import Data.Char

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    [";"]        -> showChar ';'
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i     = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print AbsLI.Ident where
  prt _ (AbsLI.Ident i) = doc $ showString $ i

instance Print AbsLI.Program where
  prt i e = case e of
    AbsLI.Prog functions -> prPrec i 0 (concatD [prt 0 functions])

instance Print AbsLI.Function where
  prt i e = case e of
    AbsLI.Fun type_ id decls stms -> prPrec i 0 (concatD [prt 0 type_, prt 0 id, doc (showString "("), prt 0 decls, doc (showString ")"), doc (showString "{"), prt 0 stms, doc (showString "}")])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print AbsLI.Decl where
  prt i e = case e of
    AbsLI.Dec type_ id -> prPrec i 0 (concatD [prt 0 type_, prt 0 id])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [AbsLI.Stm] where
  prt = prtList

instance Print [AbsLI.Function] where
  prt = prtList

instance Print [AbsLI.Decl] where
  prt = prtList

instance Print [AbsLI.Exp] where
  prt = prtList

instance Print AbsLI.Stm where
  prt i e = case e of
    AbsLI.SDec decl -> prPrec i 0 (concatD [prt 0 decl, doc (showString ";")])
    AbsLI.SAss id exp -> prPrec i 0 (concatD [prt 0 id, doc (showString "="), prt 0 exp, doc (showString ";")])
    AbsLI.SBlock stms -> prPrec i 0 (concatD [doc (showString "{"), prt 0 stms, doc (showString "}")])
    AbsLI.SWhile exp stm -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 exp, doc (showString ")"), prt 0 stm])
    AbsLI.SReturn exp -> prPrec i 0 (concatD [doc (showString "return"), prt 0 exp, doc (showString ";")])
    AbsLI.SIf exp stm1 stm2 -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 exp, doc (showString ")"), doc (showString "then"), prt 0 stm1, doc (showString "else"), prt 0 stm2])
    AbsLI.SRepeat stm exp -> prPrec i 0 (concatD [doc (showString "repeat"), prt 0 stm, doc (showString "until"), doc (showString "("), prt 0 exp, doc (showString ")"), doc (showString ";")])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print AbsLI.Exp where
  prt i e = case e of
    AbsLI.EOr exp1 exp2 -> prPrec i 1 (concatD [prt 1 exp1, doc (showString "||"), prt 2 exp2])
    AbsLI.EAnd exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString "&&"), prt 3 exp2])
    AbsLI.ENot exp -> prPrec i 3 (concatD [doc (showString "!"), prt 3 exp])
    AbsLI.ECon exp1 exp2 -> prPrec i 4 (concatD [prt 4 exp1, doc (showString "++"), prt 5 exp2])
    AbsLI.EAdd exp1 exp2 -> prPrec i 4 (concatD [prt 4 exp1, doc (showString "+"), prt 5 exp2])
    AbsLI.ESub exp1 exp2 -> prPrec i 4 (concatD [prt 4 exp1, doc (showString "-"), prt 5 exp2])
    AbsLI.EMul exp1 exp2 -> prPrec i 5 (concatD [prt 5 exp1, doc (showString "*"), prt 6 exp2])
    AbsLI.EDiv exp1 exp2 -> prPrec i 5 (concatD [prt 5 exp1, doc (showString "/"), prt 6 exp2])
    AbsLI.Call id exps -> prPrec i 6 (concatD [prt 0 id, doc (showString "("), prt 0 exps, doc (showString ")")])
    AbsLI.EInt n -> prPrec i 7 (concatD [prt 0 n])
    AbsLI.EVar id -> prPrec i 7 (concatD [prt 0 id])
    AbsLI.EStr str -> prPrec i 7 (concatD [prt 0 str])
    AbsLI.ETrue -> prPrec i 7 (concatD [doc (showString "true")])
    AbsLI.EFalse -> prPrec i 7 (concatD [doc (showString "false")])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print AbsLI.Type where
  prt i e = case e of
    AbsLI.Tbool -> prPrec i 0 (concatD [doc (showString "bool")])
    AbsLI.Tint -> prPrec i 0 (concatD [doc (showString "int")])
    AbsLI.Tvoid -> prPrec i 0 (concatD [doc (showString "void")])
    AbsLI.TStr -> prPrec i 0 (concatD [doc (showString "String")])
    AbsLI.TFun function -> prPrec i 0 (concatD [prt 0 function])
