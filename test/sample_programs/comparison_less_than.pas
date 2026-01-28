program LTExample;

var 
    i: integer;
begin
    i := 10;

    // False
    if (i < 8) then
        i := 30;

    // True
    if (i < 100) then
        i := i + 10; // i := 20

    // False
    if (i <= 18) then
        i := i + 5 
    // True
    else if (i <= 21) then
        i := i + 2; // i := 22

    // True
    if (i <= 22) then
        i := i + 1; // i := 23


    halt(i); // should be 23
end.
