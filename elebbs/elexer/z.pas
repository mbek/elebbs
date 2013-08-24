program z;
var i: integer;
    j: integer;
    k: integer;
    whee: string;
    hugewhee : string;
begin
 for i := 1 to 100000 do
  begin
    hugewhee := '';

     for j := 1 to 100 do
       begin
        whee := '';

         for k :=   1 to 10 do
           begin
              whee := whee + 'a';
           end;

        hugewhee := hugewhee + whee;
       end;
  end;

writeln(i, ' / ', j,  ' / ', k);
end.
