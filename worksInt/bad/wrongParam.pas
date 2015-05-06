//zła liczba argumentów
program
var x := 2;
var y := 3;
var z;
procedure dziel(var x, y)
begin
	x := y/2;
end
call dziel(x,y,z);
print x;
