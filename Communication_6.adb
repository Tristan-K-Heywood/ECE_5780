with Ada.Text_IO;
use  Ada.Text_IO;

with Text_Io;
use  Text_Io;

with Ada.Calendar;
use  Ada.Calendar;

with Ada.Numerics.Discrete_Random;

procedure Communication_6 is 

    function rand(max : in Integer) return Integer is
        package Random_Int is new Ada.Numerics.Discrete_Random(Integer);
        G : Random_Int.Generator;
        x : Integer;

    begin
        Random_Int.reset(G);
        x := Random_Int.Random(G, 1, max);
        return x;
    end rand;

    type list is array (0..9) of Integer;


    protected Buff is
        entry push(Num: in Integer);
        entry pop(Num: out Integer);
        entry level(L : out Integer);
        entry retire;
    private
        buff_level : Integer := 0;
        i : Integer := 0;
        
        fifo : list := (0,0,0,0,0,0,0,0,0,0);
    end Buff;

    protected body Buff is
        entry push(Num: in Integer)             -- push an integer into the fifo
            when (buff_level < 10 and i < 9) is  -- protected against overflow
            begin
                fifo(i) := Num;                 -- push to bottom of fifo
                buff_level := buff_level + 1;   -- increase fifo level
                i := i + 1;                     -- increase idx for next push
            end push;

        entry pop(Num: out Integer)             -- take the head out of the fifo
            when (buff_level > 0 and i > 0) is  -- protected against underflow
            begin
                Num := fifo(0);                 -- head is output
                buff_level := buff_level - 1;   -- decrease fifo level
                i := i - 1;                     -- decrement idx for next push

                for j in 1..9 loop              -- shift the fifo up
                    fifo(j-1) := fifo(j);   
                end loop;
            end pop;

        entry level(L : out Integer) 
            when True is
            begin
                L := buff_level;
            end level;

        entry retire 
            when True is
            begin
                Put_Line("      This is BUFFER, signing off");
                fifo := (0,0,0,0,0,0,0,0,0,0);
            end retire;
    end Buff;


    task Prod is
        entry init;
        entry retire;
    end Prod;

    task body Prod is 
        alive : Boolean := false;
        level : Integer := 9;
        start_time : Duration;
        wait_time : Float;
        rand_num : Integer;

    begin
        loop
            select
                accept init do
                    alive := true;
                end init;

            or 
                accept retire do
                    alive := false;
                end retire;
            end select;

            while alive loop
                start_time := Ada.Calendar.Seconds(Ada.Calendar.Clock);
                wait_time := Float(rand(10)) / 10.0; -- generate a random time between 1 - 10 tenths of a second
                loop
                    exit when Ada.Calendar.Seconds(Ada.Calendar.Clock) >= start_time + Duration(wait_time);     
                end loop;

                Buff.level(level);
                Put_Line("Waiting for DROP");
                while (level >= 9) loop
                    
                    Buff.level(level);
                end loop;
                Put_Line("  Finished Waiting for DROP");
                        
                rand_num := rand(25);

                Buff.push(rand_num);
                Put_Line("Placed number into fifo: " & Integer'Image(rand_num));

            end loop;
        end loop;
    end Prod;


    task Con is
        entry init;
    end Con;

    task body Con is 
        sum : Integer := 0;
        x : Integer := 0;
        level : Integer := 0;
        start_time : Duration;
        wait_time : Float;

    begin
        loop
            select
                accept init do
                    Prod.init;
                end init;
            end select;

            while (sum < 100) loop
                start_time := Ada.Calendar.Seconds(Ada.Calendar.Clock);
                wait_time := Float(rand(10)) / 10.0;
                loop
                    exit when Ada.Calendar.Seconds(Ada.Calendar.Clock) >= start_time + Duration(wait_time);     
                end loop;

                Buff.level(level);
                Put_Line("Waiting for fifo level to rise");
                while (level <= 0) loop
                    Buff.level(level);
                end loop;
                Put_Line("  Finished waiting for RISE");

                Buff.pop(x);
                sum := sum + x;
                Put_Line("Sum is now: " & Integer'Image(sum));
            end loop;
            
            Put_Line("      GO TO SLEEP");
            Buff.retire;
            Prod.retire;
            
        end loop;
    end Con;

begin
    Con.init;

end Communication_6;