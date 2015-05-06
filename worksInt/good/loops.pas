//petle
program
var x;
print "przed forem w dol";
for x:=4 downto 1 do
begin
	print x;
end

print "przed forem w gore";
for x:=1 to 10 do
begin
	print x;
	x := x+3;
end

print "przed whilem";
while (x<20 and 1=1) do
begin
	x := x + 3;
	print x;
end
