module Auto( Auto
           , leftA
           ) where
import Data.List

data Auto a q = A { states      :: [q]
                  , initStates  :: [q]
                  , isAccepting :: q -> Bool
                  , transition  :: q -> a -> [q]
                  }
				  
changeLeft :: [q] -> [(Either q r)]
changeLeft [] = []
changeLeft (x:xs) = (Left x):(changeLeft xs)

changeRight :: [q] -> [(Either r q)]
changeRight [] = []
changeRight (x:xs) = (Right x):(changeRight xs)

leftA :: (Eq q, Eq r) => Auto a q -> Auto a (Either q r) 
leftA (A s ini ac t) = (A st init acc tr) where	
	st = changeLeft s
	init = changeLeft ini
	acc s = elem s st
	tr (Left s) w = changeLeft (t s w) 

rightA :: (Eq q, Eq r) => Auto a q -> Auto a (Either r q) 
rightA (A s ini ac t) = (A st init acc tr) where	
	st = changeRight s
	init = changeRight ini
	acc s = elem s st
	tr (Right s) w = changeRight (t s w)
	
thenEitherA :: (Eq q1, Eq q2) => Auto a (Either q1 q2) -> Auto a (Either q1 q2) -> Auto a (Either q1 q2)
thenEitherA (A s1 ini1 ac1 t1) (A s2 ini2 ac2 t2) = (A st init acc tr) where
	st = s1 ++ s2
	init = ini1
	acc = ac2
	tr s w = if (not (t1 s w == [])) then
				if ac1 s then
					(t1 s w) ++ ini2 
				else
					t1 s w
			else
				if (not (t2 s w == [])) then
					t2 s w 
				else 
					[]
					
thenA :: (Eq q1, Eq q2) => Auto a q1 -> Auto a q2 -> Auto a (Either q1 q2)
thenA (A s1 ini1 ac1 t1) (A s2 ini2 ac2 t2) = thenEitherA (leftA (A s1 ini1 ac1 t1)) (rightA (A s2 ini2 ac2 t2))


		