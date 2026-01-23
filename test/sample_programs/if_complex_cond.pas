program IfExample;

var 
    i : integer;
    b : boolean;
begin
    i := 10;
    b := false;

    if (b or False) then
        i := 30
    else if (True and (not b)) then
        i := 40;

    halt(i); 
end.
