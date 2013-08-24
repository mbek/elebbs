uses crt, wordstr;

var
    cmd : ShortString;

    cmd2: ShortString;
begin
    cmd2 := 'whee whee2';
    cmd := '"foo bar" "abc" "def" "ghi"';

    writeln(cmd = cmd);
    WriteLn(WordCount(cmd, defExtractWord, True) = 4);
    writeln(FirstWord(cmd, defExtractWord, True) = 'foo bar');
    writeln(cmd = '"abc" "def" "ghi"');
    WriteLn(FirstWord(cmd2, defExtractWord, true) = 'whee');
    WriteLn(cmd2 = 'whee2');
    readln;

end.
