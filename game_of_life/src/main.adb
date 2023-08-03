
with life; use life;

procedure main is
   s : State := life.Create_State(50);
begin
   loop
      Clear_Screen;
      life.Print_State(s);
      life.Next_State(s);
      delay 0.1;
   end loop;
end main;
