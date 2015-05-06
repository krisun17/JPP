//rekurencja
program
var x := 2;
var y := 1;
procedure rec(var x, y)
begin
	x := x + y;
	if (x < 10) then
		call rec(x,y)
	fi
end
print "przed";
print x;
call rec(x,y);
print "po";
print x;
