with Ada.Text_IO;
use  Ada.Text_IO;

with Text_Io;
use  Text_Io;

with Ada.Calendar;
use  Ada.Calendar;


procedure Communication is 

   function rando (max : integer) return Integer is         
      type r_Range is new Integer range 1..max;                  
      package pack_Random is new Ada.Numerics.Discrete_Random(r_Range);
      use pack_Random;
    
      num : r_range;
      gen : generator;

   begin
      reset(gen);
      num := random(gen); 
      return num;                    
   end rando;

    task Buff is
        entry push;
        entry retire;
        entry level;
    end Buff;

    task body Buff is 
    begin
    end Buff;

    task Con is
        entry pop;
        entry empty;
    end Con;

    task body Con is 
    begin
    end Con;

    task Prod is
        entry init;
        entry retire;

    end Prod;

    task body Prod is 
        alive : Boolean := false;
        start_time : Duration;
        wait_time : Integer;

    begin
        loop
            select
                accept init do
                    alive := true;
                    
                    while alive loop
                        start_time := Ada.Calendar.Seconds(Ada.Calendar.Clock);
                        wait_time := rando/10;
                        loop
                            exit when Ada.Calendar.Seconds(Ada.Calendar.Clock) >= start_time + wait_time;     
                        end loop;

                    end loop;
                end init;
            or 
                accept retire do
                    alive := false;
                end retire;
            end select;
        end loop;
    end Prod;

begin

    Prod.init;
    Con.init;
    loop

    end loop;
end Communication;