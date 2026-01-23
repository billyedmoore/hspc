program IfExample;

var 
    i : integer;
begin
    i := 10;

    if (False) then
        i := 30
    else if (true) then
        i := 40;

    halt(i); 
end.
