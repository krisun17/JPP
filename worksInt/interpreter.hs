module Main where

import System.IO
import System.Environment ( getArgs, getProgName )

import Lexpastiny
import Parpastiny
import Skelpastiny
import Printpastiny
import Abspastiny

import ErrM

import qualified Data.Map as Map

import Interpret

main :: IO ()
main = do
    (fn:_) <- getArgs
    file <- openFile fn ReadMode
    s <- hGetContents file
    case pPrg (myLexer s) of
        Bad s -> putStrLn s
        Ok i -> do
        	mapM_ putStrLn (evalProg i)
