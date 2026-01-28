program WhileExample;

var 
    i : integer;
begin
    i := 0;

    while i < 1000 do
        i := i + 1;

    i := i div 10; // i := 100

    halt(i); 
end.
