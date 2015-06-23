import System.Environment   
import Data.List.Split
import Data.List
import Data.Char
import Data.Maybe
import Auto

	
main = do
	args <- getArgs
	case args of
		[f] -> readFile f >>= printOutput
		otherwise -> putStrLn "Wrong argument, format: ./RunAuto filePath"

printOutput :: String -> IO ()
printOutput s = putStrLn (parseAuto (filter (\x -> length x > 0) (lines s)))

---------------------preparing for parsing-----------------------------------

parseAuto :: [String] -> String
parseAuto s =	if (length s < 4) then 
					"BAD INPUT: incorrect number of lines"
				else 
					if (isNotNumberS (head s)) then 
						"BAD INPUT: incorrect number of states"
					else 
						parseLinesToAuto (tail s) (read (head s))

parseLinesToAuto :: [String] -> Int -> String 
parseLinesToAuto (b:c:xs) n = 	if ((incorrectAuto init acc tr) || (any isWrongCharacter w)) then 
									"BAD INPUT: wrong format of auto or word"
								else 
									show (accepts (fromLists [1..n] (fromMaybe [] init) (fromMaybe [] acc) (fromMaybe [] tr)) w)
								where 	init = parseToList b n;
										acc = parseToList c n;
										tr = getTransitions (tail (reverse xs)) n;
										w = (head (reverse xs))
										
-------------------------parsing transitions--------------------------------------------------------

getTransitions :: [String] -> Int -> Maybe [(Int,Char,[Int])]
getTransitions [] n = Just []
getTransitions (x:xs) n = let l = (parseToTransition (words x) n)
							in 	if isNothing l then 
									Nothing 
								else 
									Just ((fromMaybe [] l) ++ (fromMaybe [] (getTransitions xs n)))	

parseToTransition :: [String] -> Int -> Maybe [(Int, Char, [Int])]
parseToTransition (a:b:xs) n = if (isWrongNumber n a) then 
										Nothing 
									else 
										if (any isWrongCharacter b) then 
											Nothing 
										else 
											if (any (isWrongNumber n) xs) then 
												Nothing 
											else 
												Just [((read a),c, (rmdups (map read xs))) | c <- b]

parseTransition :: [String] -> Int -> Maybe [(Int, Char, [Int])]
parseTransition s n = 	if (length s) >= 2 then 
							parseToTransition s n 
						else 
							Nothing
	
----------------------------parsing lists of states--------------------------------

parseToList :: String -> Int -> Maybe [Int]
parseToList s n = 	let l = (filter (/=']') (filter (/='[') (filter (/=' ') s))) 
					in 
						parseListString (splitOn "," l) n where 
						parseListString :: [String] -> Int -> Maybe [Int]
						parseListString s n = 	if (any (isWrongNumber n) s) then
													Nothing 
												else 
													Just (rmdups (map read s))
													
-----------------------other functions--------------------------------------------
		
incorrectAuto :: Maybe [Int] -> Maybe [Int] -> Maybe [(Int,Char,[Int])] -> Bool 
incorrectAuto a b c = isNothing a || isNothing b || isNothing c	

isNotNumberS :: String -> Bool
isNotNumberS [] = False
isNotNumberS (x:xs) = (not (isDigit x)) || (isNotNumberS xs)

isWrongNumber :: Int -> String -> Bool 
isWrongNumber n s = if (isNotNumberS s) then 
						True 
					else 
						if ((read s) <= n) then 
							False 
						else 
							True 
							
isWrongCharacter :: Char -> Bool 
isWrongCharacter c = not ((isAlpha c) && (isUpper c))
 
rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort
							







   
