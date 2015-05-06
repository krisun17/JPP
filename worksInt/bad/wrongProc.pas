//niezainicjowana nazwa procedury
program
var x := 2;
var y := 3;
procedure dziel(var x, y)
begin
	x := y/2;
end
call dzieli(x,y);
print x;
