program IfExample;

var 
    i : integer;
begin
    i := 10;

    if (True) then
        begin
            i := 10;
            i := 20;
            i := 15;
       end;

    halt(i); 
end.
