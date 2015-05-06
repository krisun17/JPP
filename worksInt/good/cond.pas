//instrukcje warunkowe
program
var x := 5;
if (x = 5) then
begin
	x := 6;
	if (x=6) then
	begin 
		x := 4;
	end
	fi
end
else
	x := 1;
fi
print x;

if (x=6) then
begin 
	x := 4;
end
fi
print x;
