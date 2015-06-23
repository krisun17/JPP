module Auto( Auto
           , accepts
           , emptyA, epsA, symA
           , leftA, rightA, sumA, thenA
           , fromLists, toLists
           ) where
import Data.List

data Auto a q = A { states      :: [q]
                  , initStates  :: [q]
                  , isAccepting :: q -> Bool
                  , transition  :: q -> a -> [q]
                  }
	
---------------accepts---------------------------------------
									
accepts :: (Eq q) => Auto a q -> [a] -> Bool
accepts (A st init acc tr) s = any acc (f s init []) where 
	f [] st wyn = st
	f (w:ws) [] wyn = f ws wyn []
	f (w:ws) (s:st) wyn = f (w:ws) st (union wyn (tr s w))

------------emptyA----------------

emptyA :: Auto a ()
emptyA = A [] [] (\q -> False) (\q a -> [])

----------epsA---------------------
	
epsA :: Auto a ()
epsA = A [()] [()] (\q -> q == ()) (\q a -> [])

-------symA-------------------------------------

symA :: Eq a => a -> Auto a Bool
symA w = (A [False, True] [False] (\q -> q) tr) where
	tr False x
		| x == w	= [True]
		| otherwise = []
	tr True x = []

---------------leftA------------------------------------------
	
leftA :: Auto a q -> Auto a (Either q r) 
leftA (A s ini ac t) = (A st init acc tr) where	
	st = map (Left) s
	init = map (Left) ini
	acc = either ac (\x->False)
	tr (Left s) w = map (Left) (t s w)
	tr (Right s) w = []

-----------------rightA----------------------------------------
	
rightA :: Auto a q -> Auto a (Either r q) 
rightA (A s ini ac t) = (A st init acc tr) where	
	st = map (Right) s
	init = map (Right) ini
	acc = either (\x->False) ac
	tr (Right s) w = map (Right) (t s w)
	tr (Left s) w = []
	
-------------------fromLists-------------------------------------------
		
getElem :: [(q,a,[q])] -> [q]
getElem [] = []
getElem [(x,y,z)] = z
			
fromLists :: (Eq q, Eq a) => [q] -> [q] -> [q] -> [(q,a,[q])] -> Auto a q
fromLists s ini ac t = (A s ini acc tr) where
	acc x = elem x ac
	tr x y = getElem (filter (\(st,w,_) -> (st == x) && (w==y)) t) 
				
	
--------------------toLists---------------------------------------------------
								
toLists :: (Enum a,Bounded a) => Auto a q -> ([q],[q],[q],[(q,a,[q])])
toLists (A s ini ac t) = (s, ini, acc, tr) where
	letters :: (Enum a,Bounded a) => [a]
	letters = [minBound..]
	acc = filter ac s
	tr = [(x,y,(t x y)) | x <- s, y <- letters]
								
------------------------thenA---------------------------------------------------------------------------

thenA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
thenA (A sL iniL acL tL) (A sR iniR acR tR) = (A st init ac2 tr) where 
	(A s1 ini1 ac1 t1) = leftA (A sL iniL acL tL)
	(A s2 ini2 ac2 t2) = rightA (A sR iniR acR tR)
	st = s1 ++ s2
	init =  if any ac1 ini1 then 
				ini1 ++ ini2 
			else 
				ini1
	tr (Left x) w = let l = t1 (Left x) w
						in	if (any ac1 l) then 
								l ++ ini2
							else 
								l
	tr (Right x) w = t2 (Right x) w
	
	
----------------------------sumA---------------------------------------------
				
sumA :: Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
sumA (A sL iniL acL tL) (A sR iniR acR tR) = (A st init acc tr) where 
	(A s1 ini1 ac1 t1) = leftA (A sL iniL acL tL)
	(A s2 ini2 ac2 t2) = rightA (A sR iniR acR tR)
	st = s1 ++ s2
	init = ini1 ++ ini2
	acc s = (ac1 s) || (ac2 s)
	tr x y = (t1 x y) ++ (t2 x y)
	
------------------------show-----------------------------------------------

instance (Show a, Enum a, Bounded a, Show q, Eq q) => Show (Auto a q) where
				show (A st init acc tr) = show (toLists (A st init acc tr))
