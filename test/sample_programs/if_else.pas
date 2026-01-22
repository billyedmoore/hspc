program IfExample;

var 
    i : integer;
begin
    i := 10;

    if (False) then
        i := 30
    else 
        i := 40;

    halt(i); 
end.
