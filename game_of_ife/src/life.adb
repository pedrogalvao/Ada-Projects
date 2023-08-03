with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;

with Ada.Strings.Unbounded.Text_IO;
use Ada.Strings.Unbounded.Text_IO;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

package body life is
   
   procedure Next_State(s: in out State) is 
      Rows : Integer := s'Length(1);
      Cols : Integer := s'Length(1);
      S2: State(1..Rows, 1..Cols);
      neighbors_count : Integer;
   begin
      for i in 1..Rows loop
         for j in 1..Cols loop
            neighbors_count := 0;
            for dx in -2..0 loop
               for dy in -2..0 loop
                  if s((i + dx) mod Rows +1, (j + dy) mod Cols+1) = On then
                     neighbors_count := neighbors_count + 1;
                  end if;
               end loop;
               if s(i, j) = On and (neighbors_count = 3 or neighbors_count = 4) then
                  S2(i, j) := On;
               elsif s(i, j) = Off and neighbors_count = 3 then
                  S2(i, j) := On;
               else
                  S2(i, j) := Off;
               end if;
            end loop;
         end loop;
      end loop;
      s := S2;
   end Next_State;
   
   procedure Print_State(s: in State) is
      lopt : Cell;
      Rows : constant Positive := s'Length(1);
      Cols : constant Positive := s'Length(2);
   begin
      for I in 1..Rows loop
         for J in 1..Cols loop
            lopt := s(I, J);
            case lopt is 
            when On => Put(" O");
            when Off => Put(" .");
            end case;
         end loop;
         Ada.Text_IO.New_Line;
      end loop;
      Ada.Text_IO.New_Line;
   end Print_State;

   procedure Clear_Screen is
   begin
      put(ESC & "[2J");
   end;

   function Create_State(size: Natural) return State is
      Arr : State(1..size, 1..size);
   begin
      for I in 1..size loop
         for J in 1..size loop
            if (I - J) mod 3 /= 0 and (I + J) mod 5 /= 0 and I mod 7 /= 0 then
               Arr(I, J) := On;
            else
               Arr(I, J) := Off;
            end if;
         end loop;
      end loop;
      return Arr;
   end Create_State;
end life;
