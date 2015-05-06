module Interpret where

import Abspastiny
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe(fromMaybe)

-------------typy i monady pomocnicze-----------------------------------

type Proc = ([Adec], Stm)
data Val =
      VInt Integer
    | VBool Bool
    deriving (Eq, Ord, Show)

type Loc = Integer
type Env = M.Map String Loc
type Store = M.Map Loc (Either Proc Val)

----------wyjście programu będzie listą wartości zwracanych przez print-

type Output = [String]

---------monada stanu i środowiska, w State trzymamy aktualny stan (Store),
---------wyjście oraz następną nieużywaną lokację-----------------------

type MI a = StateT (Store, Output, Loc) (Reader Env) a     

---------funkcje pomocniczne--------------------------------------------

alloc :: MI Loc
alloc = do
	(s, o, l) <- get
	put(s, o, l+1)
	return l
	
getStore :: MI Store
getStore = do
    (s, _, _) <- get
    return s
    
putStore :: Store -> MI ()
putStore s = do
    (_,o,l) <- get
    put (s,o,l)
    
getObject :: String -> MI (Either Proc Val)
getObject x = do
    e <- ask
    s <- getStore
    let loc = fromMaybe (error "not initialized variable") (M.lookup x e)
    let obj = fromMaybe (error "not initialized variable") (M.lookup loc s)
    return obj

printOut :: String -> MI ()
printOut x = do
	(s,o,l) <- get
	put (s,x:o,l)
	
getVal :: String -> MI Val
getVal x = do
    Right v <- getObject x
    return v

getProc :: String -> MI Proc
getProc x = do
    Left p <- getObject x
    return p

evalInt :: Exp -> MI Integer
evalInt e = do
    VInt n <- eval e
    return n

evalBool :: Exp -> MI Bool
evalBool e = do
    VBool b <- eval e
    return b
    

------------wyrazenia---------------------------------------------------
  
eval :: Exp -> MI Val

eval (EInt a) = return (VInt a)

eval (Evar (Ident v)) = do
	n <- (getVal v)
	return n

-----------logiczne-----------------------------------------------------

eval (EAnd e1 e2) = do
	w1 <- (evalBool e1)
	w2 <- (evalBool e2)
	return (VBool (w1 && w2))
	
eval (EOr e1 e2) = do
	w1 <- (evalBool e1)
	w2 <- (evalBool e2)
	return (VBool (w1 || w2))

eval (ENot e) = do
	w <- (evalBool e)
	return (VBool (not w))
	
--------porównania------------------------------------------------------

eval (EEq e1 e2) = do
	w1 <- (evalInt e1)
	w2 <- (evalInt e2)
	if (w1 == w2) then 
		return (VBool True)
	else
		return (VBool False)
		
eval (Eneq e1 e2) = do
	w1 <- (evalInt e1)
	w2 <- (evalInt e2)
	if (w1 /= w2) then 
		return (VBool True)
	else
		return (VBool False)
		
eval (Elt e1 e2) = do
	w1 <- (evalInt e1)
	w2 <- (evalInt e2)
	if (w1 < w2) then 
		return (VBool True)
	else
		return (VBool False)
		
eval (Egt e1 e2) = do
	w1 <- (evalInt e1)
	w2 <- (evalInt e2)
	if (w1 > w2) then 
		return (VBool True)
	else
		return (VBool False)

eval (Eelt e1 e2) = do
	w1 <- (evalInt e1)
	w2 <- (evalInt e2)
	if (w1 <= w2) then 
		return (VBool True)
	else
		return (VBool False)
		
eval (Eegt e1 e2) = do
	w1 <- (evalInt e1)
	w2 <- (evalInt e2)
	if (w1 >= w2) then 
		return (VBool True)
	else
		return (VBool False)

-----------arytmetyczne-------------------------------------------------
		
eval (EAdd e1 e2) = do
	w1 <- (evalInt e1)
	w2 <- (evalInt e2)
	return (VInt (w1 + w2))
	
eval (ESub e1 e2) = do
	w1 <- (evalInt e1)
	w2 <- (evalInt e2)
	return (VInt (w1 - w2))
	
eval (Einc (Evar (Ident x))) = do
	w <- (evalInt (Evar (Ident x)))
	interpretAss (Assign (Ident x) (EInt (w+1)))
	return (VInt (w + 1))
	
eval (Einc e) = do
	w <- (evalInt e)
	return (VInt (w + 1))
	
eval (Edec (Evar (Ident x))) = do
	w <- (evalInt (Evar (Ident x)))
	interpretAss (Assign (Ident x) (EInt (w-1)))
	return (VInt (w - 1))
		
eval (Edec e) = do
	w <- (evalInt e)
	return (VInt (w - 1))
	
eval (Ediv e1 e2) = do
	w1 <- (evalInt e1)
	w2 <- (evalInt e2)
	if (w2 == 0) then error "division by zero is prohibited" 
	else return (VInt (quot w1 w2))
	
eval (EMul e1 e2) = do
	w1 <- (evalInt e1)
	w2 <- (evalInt e2)
	return (VInt (w1*w2))
	

------------przypisanie-------------------------------------------------

interpretAss :: Ass -> MI ()

interpretAss (Assign (Ident x) e) = do
	env <- ask
	let loc = fromMaybe (error "not initialized variable") (M.lookup x env)
	w <- (evalInt e)
	st <- getStore
	let s = (M.insert loc (Right (VInt w)) st)
	putStore s

------------instrukcje--------------------------------------------------

interpret :: Stm -> MI ()

interpret Sempty = return ()
	
interpret (Sass a) = do 
	s <- (interpretAss a)
	return s
	
interpret (Selif e s1 s2) = do
	con <- (evalBool e)
	if (con) then interpret s1 else interpret s2
	
interpret (Sif e s) = do
	con <- (evalBool e)
	if (con) then interpret s else interpret Sempty
	
interpret (Swhile e s) = do
	con <- (evalBool e)
	if (con) then do {interpret s; interpret (Swhile e s)} 
	else interpret Sempty
	
interpret (Sfort (Assign (Ident x) e1) e2 s) = do
	interpretAss (Assign (Ident x) e1)
	wx <- evalInt (Evar (Ident x))
	w2 <- (evalInt e2)
	w1 <- (evalInt (Einc e1))
	if (wx <= w2) then do 
	{interpret s; interpret (Sfort (Assign (Ident x) (EInt w1)) e2 s)}
	else interpret Sempty
	
interpret (Sford (Assign (Ident x) e1) e2 s) = do
	interpretAss (Assign (Ident x) e1)
	wx <- evalInt (Evar (Ident x))
	w2 <- (evalInt e2)
	w1 <- (evalInt (Edec e1))
	if (wx >= w2) then do 
	{interpret s; interpret (Sford (Assign (Ident x) (EInt w1)) e2 s)}
	else interpret Sempty
	
interpret (SBlock []) = interpret Sempty

interpret (SBlock (s:ds)) = do
	interpret s
	interpret (SBlock ds)
	
interpret (Scall (Ident p) args) = do
	e <- ask
	(adec, stm) <- getProc p
	e1 <- (prepareEnv e args adec)
	local (const e1) (interpret stm)
	      
interpret (Sprint x) = do
	n <- (eval x)
	printOut (show n)
	
interpret (Sprintstr x) = do
	printOut x
	
-------przygotowanie środowiska do wywołania procedury------------------

prepareEnv :: Env -> [Exp] -> [Adec] -> MI Env
prepareEnv env exps args =
    prep env exps args where
        prep :: Env -> [Exp] -> [Adec] -> MI Env
        prep currEnv [] [] = do
            return currEnv
        prep currEnv (exp:exps) ((ADecv (Ident x)):ps) = do
            e1 <- local (const currEnv) (evalDec (VDeclass (Ident x) exp))
            e2 <- (prep e1 exps ps)
            return e2
        prep currEnv ((Evar (Ident var)):exps) ((ADecr (Ident x)):ps) = do
            e <- ask
            let (Just loc) = M.lookup var e
            let newEnv = M.insert x loc currEnv
            e2 <- (prep newEnv exps ps)
            return e2

---------lista intrukcji------------------------------------------------

interpretList :: [Stm] -> MI ()

interpretList [] = return ()
interpretList (s:ss) = do
	interpret s
	interpretList ss
	
---------------deklaracje-----------------------------------------------

evalDec :: Dec -> MI Env

evalDec (VDecl (Ident x)) = do
	st <- getStore
	env <- ask
	fl <- alloc
	let e = M.insert x fl env
	let s = M.insert fl (Right (VInt 0)) st
	putStore s
	en <- ask
	let loc = fromMaybe (error "not initialized variable") (M.lookup x en)
	return e
	
evalDec (VDeclass (Ident x) e) = do
	st <- getStore
	w <- (evalInt e)
	env <- ask
	fl <- alloc
	let e = M.insert x fl env
	let s = M.insert fl (Right (VInt w)) st
	en <- ask
	let loc = fromMaybe (error "not initialized variable") (M.lookup x en)
	putStore s
	return e
	
evalDec (PDecl (Ident p) adec s) =  do
	st <- getStore
	env <- ask
	fl <- alloc
	let e = M.insert p fl env
	let ns = M.insert fl (Left (adec, s)) st
	putStore ns
	return e
	
evalDecs :: [Dec] -> MI Env

evalDecs [] = do
    e <- ask
    return e
evalDecs (d:ds) = do
    e <- ask
    e1 <- evalDec d
    e2 <- local (const e1) (evalDecs ds)
    return e2
    
---------------program--------------------------------------------------

interpretProg :: Prg -> MI ()

interpretProg (Prog d s) = do
	e <- (evalDecs d)
	local (const e) (interpretList s)

evalProg :: Prg -> Output
evalProg p = 
	let ((),(_,o,_)) = runReader (runStateT (interpretProg p) (M.empty, [], 0)) M.empty
	in reverse o
		

	
	
	 
