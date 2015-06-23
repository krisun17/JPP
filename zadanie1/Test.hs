data Test q = A { states      :: [q]
                } deriving(Show)
				
transformL :: [q1] -> [Either q1 q2]
transformL l = map Left l

transformR :: [q2] -> [Either q1 q2]
transformR l = map Right l
				
