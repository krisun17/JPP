module Interpreter where

import AbsexpGram
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as M
import Data.Maybe(fromMaybe)

-------------monady pomocnicze------------------

type Proc = ([Adec], Stm)
type Val = Integer

type Loc = Integer
type Env = M.Map String Loc
type Store = M.Map Loc (Either Proc Val)

type MI a = ReaderT Env (State (Store, Loc)) a     -- mozemy traktowac monadę MI jednoczesnie jako monadę stanu i srodowiska

alloc :: MI Loc
alloc = do
	(s,l) <- get
	put(s, l+1)
	return l
	
getStore :: MI Store
getStore = do
    (s, _) <- get
    return s

evalStm :: Stm -> Store
evalStm s = 
	let (_,st) = runState (runReaderT (interpret s) (M.empty)) (M.empty,0)
	in st

------------wyrazenia--------------------------

eval :: Exp -> MI Integer

eval (EInt a) = return a

eval (Evar v) = do
	env <- ask
	let loc = fromMaybe (error "not initialized var") (M.lookup v env)
	store <- getStore
	return (fromMaybe (error "not initialized var") (M.lookup loc store))

eval (EAnd e1 e2) = do
	w1 <- (eval e1)
	w2 <- (eval e2)
	if ((w1 == 0) || (w2 == 0)) then 
		return 0
	else
		return 1
	
eval (EOr e1 e2) = do
	w1 <- (eval e1)
	w2 <- (eval e2)
	if ((w1 == 0) && (w2 == 0)) then 
		return 0
	else
		return 1

eval (ENot e) = do
	w <- (eval e)
	if (w == 0) then 
		return 1
	else
		return 0

eval (EEq e1 e2) = do
	w1 <- (eval e1)
	w2 <- (eval e2)
	if (w1 == w2) then 
		return 1
	else
		return 0
		
eval (Elt e1 e2) = do
	w1 <- (eval e1)
	w2 <- (eval e2)
	if (w1 < w2) then 
		return 1
	else
		return 0
		
eval (EAdd e1 e2) = do
	w1 <- (eval e1)
	w2 <- (eval e2)
	return (w1 + w2)
	
eval (ESub e1 e2) = do
	w1 <- (eval e1)
	w2 <- (eval e2)
	return (w1 - w2)
	
eval (Einc e) = do
	w <- (eval e)
	return (w + 1)
	
eval (Edec e) = do
	w <- (eval e)
	return (w - 1)
	
eval (Ediv e1 e2) = do
	w1 <- (eval e1)
	w2 <- (eval e2)
	return (quot w1 w2)	
	
eval (EMul e1 e2) = do
	w1 <- (eval e1)
	w2 <- (eval e2)
	return (w1*w2)
	

------------przypisanie-----------------

evalAss :: Ass -> MI ()

evalAss (Assign x e) = do
	env <- ask
	let loc = fromMaybe (error "not initialized variable") (M.lookup (Right x) env)
	w <- (eval e)
	modify (M.insert loc w)

------------instrukcje------------------

interpret :: Stm -> MI ()

interpret Sempty = return ()
	
interpret (Sass a) = do 
	s <- (evalAss a)
	return s
	
interpret (Selif e s1 s2) = do
	w <- (eval e)
	if w==0 then interpret s1 else interpret s2
	
interpret (Sif e s) = do
	w <- (eval e)
	if w==0 then interpret s else do interpret Sempty
	
interpret (Swhile e s) = do
	w <- (eval e)
	if w==0 then interpret s else do {interpret s; interpret (Swhile e s)}
	
interpret (Sfort (Assign x e1) e2 s) = do
	evalAss (Assign x e1)
	wx <- eval (Evar x)
	w2 <- (eval e2)
	w1 <- (eval (Einc e1))
	if (wx <= w2) then do {interpret s; interpret (Sfort (Assign x (EInt w1)) e2 s)}
	else interpret Sempty
	
interpret (Sford (Assign x e1) e2 s) = do
	evalAss (Assign x e1)
	wx <- eval (Evar x)
	w2 <- (eval e2)
	w1 <- (eval (Edec e1))
	if (wx >= w2) then do {interpret s; interpret (Sfort (Assign x (EInt w1)) e2 s)}
	else interpret Sempty
	
interpret (SBlock s) = interpret s

interpret (Scall p adec) = 
	undefined

interpret (Sprint x) = 
	undefined
	
---------------deklaracje------------------

evalDecv :: Vdec -> MI ()

evalDecv (VDecl x) = do
	st <- getStore
	env <- ask
	fl <- alloc
	modify(M.insert fl 0)
	M.insert (Right x) fl env
	
evalDecp :: Pdec -> MI ()

evalDecp (PDecl p adec s) = 
	undefined
	
evalDeca :: Adec -> MI ()

evalDeca (ADecr x) = 
	undefined

evalDeca (ADecv x) = 
	undefined
	
---------------program---------------------

evalProg :: Prg -> MI ()

evalProg (Prog (v:vs) (p) (s)) = do
	(evalDecv v)
	(evalProg (Prog vs p s))
	
evalProg (Prog [] (p:ps) (s)) = do
	(evalDecp p)
	(evalProg (Prog [] ps s))

evalProg (Prog [] [] (s:ss)) = do
	(interpret s)
	(evalProg (Prog [] [] ss))	

	
	
	 
