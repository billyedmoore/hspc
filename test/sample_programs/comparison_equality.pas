program EQExample;

var 
    i: integer;
begin
    i := 10;

    // False
    if (i <> 10) then
        i := 30;

    // True
    if (i = 10) then
        i := i + 10; // i := 20

    // False
    if (i = 18) then
        i := i + 5 
    // True
    else if (i = 20) then
        i := i + 2; // i := 22

    // False
    if (i <> 22) then
        i := i + 1;


    halt(i); // should be 22
end.
