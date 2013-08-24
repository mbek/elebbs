uses crt;

var ch: char;
begin
while ch <> #27 do
begin
 ch := readkey;
writeln(ord(ch), ' / ', ch);
end;
end.
