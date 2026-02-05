program WhileExample;

var 
    i : integer;
    j : integer;
begin
    i := 0;
    j := 0;

    while i < 25 do
        begin
         j := (j + 20) - 10;
         i := i + 1;
        end;


    halt(j); 
end.
