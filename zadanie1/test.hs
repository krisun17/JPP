let q = case (find (\(x,y,_) -> (x == 1) && (y==2)) l) of
			Nothig -> []
			Just (_,_,q) -> q in 
