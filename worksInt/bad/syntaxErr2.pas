//inicjalizacja zmiennej wewnątrz procedury
program
var x := 2;
var y := 3;
var z;
procedure foo(var x, y, z)
begin
	var k := 1;
	x := y/2;
	y := y*2
	z := y*3;
end
call foo(x,y,z);
print x;
