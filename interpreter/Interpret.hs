module Main where
  
import LexexpGram
import ParexpGram
import AbsexpGram
import Interpreter
  
import ErrM
  
main = do
   interact expGram
   putStrLn ""
  
expGram s = 
   let Ok e = pStm (myLexer s) 
   in show (evalStm e)
