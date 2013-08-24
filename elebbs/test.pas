program aa;
{$I compiler.inc}

uses dos, cases;

var a: string[4];
    b: SearchRec ;
begin
 b.name := 'aaaaa';
 a := supcase(b.name);

 writeln(a);
end.