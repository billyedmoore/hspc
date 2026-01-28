program GTExample;

var 
    i: integer;
    j: integer;
begin
    i := 10;
    j := 12;

    if (i > 8) then
        i := 30;

    if (j >= 12) then
        j := 10;

    halt(i+j); // should be 40
end.
