//wywołanie instrukcji przed procedurą
program
var x := 2;
var y := 3;
var z;
call dziel(x,y,z);
procedure foo(var x, y, z)
begin
	var k := 1;
	x := y/2;
	y := y*2
	z := y*3;
end
print x;
