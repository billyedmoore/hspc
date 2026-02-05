program ForExample;

var 
    i : integer;
    j : integer;
    m : integer;
begin   
    m := 0;
    for i:=1 to 4 do 
        for j:=4 downto 1 do m := m + 1;

    halt(m); 
end.
