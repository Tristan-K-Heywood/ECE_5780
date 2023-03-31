with Ada.Text_IO;
use  Ada.Text_IO;

with Text_Io;
use  Text_Io;

with Ada.Calendar;
use  Ada.Calendar;

with Ada.Numerics.Discrete_Random;
with Ada.Real_Time;

procedure Cyclic_Scheduler is
   
   vTime, F_Start, F_Curr, Before, After, cumDelay: Duration;
   periodStart: Time;
   missed_bool : Boolean;
   F3_comp : Boolean;
   Watchdog_start : Duration;
   Actually_missed : Boolean := false;
   Threshold : Duration;

   package DIO is new Text_Io.Fixed_Io(Duration); --To print Duration variables you can instantiate the generic 
						--package Text_Io.Fixed_Io with a duration type: 
						--"package DIO is new Text_Io.Fixed_Io(Duration);" 
						--The DIO package will then export, among other things, 
						--the procedure DIO.Put(D:Duration, Fore:Field, Aft:Field) 
						--to print variable D of type Duration. See an example
						--on how to use this below.
   
   --Declare F, which prints out a message when it starts and stops executing

   task Watchdog is 
      entry Go;
      entry Check;
      entry Shift;
   end Watchdog;

   task body Watchdog is 
      --Time_start : Duration;
   begin
      loop
         select
            accept Go do
               Watchdog_start := Ada.Calendar.Seconds(Ada.Calendar.Clock); 
            end Go;
         or
            accept Check do
               if F_Curr - Watchdog_start >= 0.5 and F3_comp = false and Actually_missed = False then
                  Put_Line("");
                  Put_Line("ERROR: Deadlines Missed at: ");
                  DIO.Put(vtime + cumDelay + (F_Curr - Watchdog_start));
                  Actually_missed := true;   
               --else
               --   Put_line("");
               --   Put_Line("Check passed");  
               end if;
            end Check;
         or
            accept Shift do
               null;
            end Shift;
         or
            terminate;   
         end select;
      end loop;
   end Watchdog;

   procedure missed is                          -- Uses a random number to generate a possibility in F3 missing its deadline
      type r_Range is new Integer range 1..3;   
      package pack_Random is new Ada.Numerics.Discrete_Random(r_Range);
      use pack_Random;

      num : r_range;
      gen : generator;

   begin
      reset(gen);
      num := random(gen);                       -- The chance of getting a missed deadline is 1 in 3
      if num = 3 then
         missed_bool := true;
         --Put_Line("");
         --Put_Line("Warning: Deadline will be missed ");
      else
         missed_bool := false;
      end if;
   end missed;
      

   procedure F(Title: String; Currtime: Duration; StartF: Duration; FinishF: Duration) is 
   begin
      if StartF = 0.0 and then FinishF = 0.0 then
	      Put_Line(""); --Add a new line
	      Put(Title);
	      Put_Line(" has started executing. The time is now:");
	      DIO.Put(Currtime);
         if Title = "F3" then
            missed;
            F3_comp := false;
            Watchdog.Go;
         end if;
         if Title = "F1" then 
            Actually_missed := false;
         end if;
      else
	      Put_Line("");
	      Put(Title);
	      Put_Line(" has finished executing. The time is now:");
	      DIO.Put(Currtime + (FinishF - StartF)); --Needed since time starts at 0 and FinishF and StartF are not virtual times
         if Title = "F3" then
            F3_comp := true;
         end if;
      end if;  
   end F;

begin
   vTime := 0.0;
   Before := Ada.Calendar.Seconds(Ada.Calendar.Clock);
   missed_bool := false;
   
   --Main loop
   loop
      After := Ada.Calendar.Seconds(Ada.Calendar.Clock); -- Russell: "Pretty sure After and Before are used solely for 1s period"


      If Actually_missed then
         Threshold := 2.000000000;
      else
         Threshold := 1.000000000;
      End if;
      if After - Before >= Threshold then   -- Begin execution (Execute F1) every 1 second

         --Put_Line("");
         --Put_Line("After is: ");
         --Dio.Put(After);

         cumDelay := 0.0; -- virtual time relative to start of vTime
	      vTime := vTime + (After - Before); --Needed since time starts at 0
	      F_Start := Ada.Calendar.Seconds(Ada.Calendar.Clock); --Get start time of F1
         periodStart := Ada.Calendar.Clock;
	      F(Title => "F1", Currtime => vTime, StartF => 0.0, FinishF => 0.0); --Initialize F1

	      loop --F1 starts

	         F_Curr := Ada.Calendar.Seconds(Ada.Calendar.Clock); --Get current time
	         exit when  F_Curr - F_Start >= 0.3000; --Assuming F1 takes 0.3 seconds

	      end loop; --F1 ends
         
	      --After F1 finishes executing, call the F1 procedure again to obtain the finish time
	      F(Title => "F1", Currtime => vTime, StartF => F_Start, FinishF => F_Curr);
         cumDelay := cumDelay + (F_Curr - F_Start); -- update elapsed time w/in period n


         F_Start := Ada.Calendar.Seconds(Ada.Calendar.Clock); -- Get start time of F2
         F(Title => "F2", Currtime => vTime + cumDelay, StartF => 0.0, FinishF => 0.0); -- Initialize F2

         loop -- F2 starts

	         F_Curr := Ada.Calendar.Seconds(Ada.Calendar.Clock); -- Get current time
	         exit when  F_Curr - F_Start >= 0.1500; -- Assuming F2 takes 0.15 seconds

	      end loop; -- F2 ends

         -- After F2 finishes executing, call the F2 procedure again to obtain the finish time
	      F(Title => "F2", Currtime => vTime + cumDelay, StartF => F_Start, FinishF => F_Curr);
         cumDelay := cumDelay + (F_Curr - F_Start); -- update elapsed time w/in period n


         delay until periodStart + 0.5; -- delay until 0.5 seconds after F1 begins

         F_Start := Ada.Calendar.Seconds(Ada.Calendar.Clock);
         cumDelay := cumDelay + (F_Start - F_Curr); -- update elapsed time w/in period n (we had to do nothing for a little to meet timing req)
         F(Title => "F3", Currtime => vTime + cumDelay, StartF => 0.0, FinishF => 0.0); -- Initialize F3


         loop -- F3 starts

	         F_Curr := Ada.Calendar.Seconds(Ada.Calendar.Clock); -- Get current time

            if missed_bool /= true then 
	            exit when F_Curr - F_Start >= 0.2000; -- Assuming F3 takes 0.2 seconds
               Watchdog.check;
            else
               exit when F_Curr - F_Start >= 0.5100; -- Missed deadline
               Watchdog.check;
            end if;

	      end loop; -- F3 ends

         Watchdog.check;
         F(Title => "F3", Currtime => vTime + cumDelay, StartF => F_Start, FinishF => F_Curr);
         Watchdog.check;

         
         Before := After;

         --Put_Line("");
         --Put_Line("Before is: ");
         --Dio.Put(Before);

      end if; --Every 1 second
   end loop; --Main loop
end Cyclic_Scheduler;