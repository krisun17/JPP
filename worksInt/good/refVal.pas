//przekazywanie przez zmienną i przez wartość
program
var u := 2;
var p := 3;
var r := 3;
var x;
var y;
var z;
procedure mnoz(var x, y, z)
begin
	print "wartosci zmiennych w procedurze";
	print x;
	print y;
	print z;
	y := y*2;
	z := z*2;
	x := z*y;
end
print "przed";
print u;
print p;
print r;
call mnoz(u,p,1);
print "po";
print u;
print p;
print r;
