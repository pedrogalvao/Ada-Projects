with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;

with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Strings.Unbounded.Text_IO;
use Ada.Strings.Unbounded.Text_IO;

package body snake is
   
   function Is_Collision(p: Position; s: State) return Boolean is 
      Rows : Integer := s.state_grid'Length(1);
      Cols : Integer := s.state_grid'Length(1);
   begin
      if p(1) > Rows or else
        p(2) > Cols or else
        p(1) <= 0 or else
        p(2) <= 0 or else
        Is_Snake(s.state_grid(p(1), p(2))) then
         return true;
      else
         return false;
      end if;
   end Is_Collision;
   
   procedure Next_State(s: in out State) is 
      Rows : Integer := s.state_grid'Length(1);
      Cols : Integer := s.state_grid'Length(1);
      next_head_position : Position;
      next_tail_position : Position;
      Tail_Moved : Boolean := False;
      Head_Cell : Cell;
   begin
      case s.snake_direction is
         when Up =>
            next_head_position := (s.snake_head(1)-1, s.snake_head(2));
         when Down =>
            next_head_position := (s.snake_head(1)+1, s.snake_head(2));
         when Left =>
            next_head_position := (s.snake_head(1), s.snake_head(2)-1);
         when Right =>
            next_head_position := (s.snake_head(1), s.snake_head(2)+1);
      end case;
      
      if Is_Collision(next_head_position, s) then 
         s.game_over := True;
         return;
      end if;
      
      if s.state_grid(next_head_position(1), next_head_position(2)) = Empty then
         case s.state_grid(s.snake_tail(1), s.snake_tail(2)) is
            when Snake_Up => 
               next_tail_position := (s.snake_tail(1)-1, s.snake_tail(2));
            when Snake_Down => 
               next_tail_position := (s.snake_tail(1)+1, s.snake_tail(2));
            when Snake_Left => 
               next_tail_position := (s.snake_tail(1), s.snake_tail(2)-1);
            when Snake_Right => 
               next_tail_position := (s.snake_tail(1), s.snake_tail(2)+1);
            when others => 
               s.game_over := True;
               Put("ERROR");
         end case;
         s.state_grid(s.snake_tail(1), s.snake_tail(2)) := Empty;
         s.snake_tail := next_tail_position;
      end if;
      
      case s.snake_direction is 
         when Up =>
            Head_Cell := Snake_Up;
         when Down =>
            Head_Cell := Snake_Down;
         when Left =>
            Head_Cell := Snake_Left;
         when Right =>
            Head_Cell := Snake_Right;
      end case;
      s.state_grid(s.snake_head(1), s.snake_head(2)) := Head_Cell;
      s.snake_head := next_head_position;
      s.state_grid(s.snake_head(1), s.snake_head(2)) := Head_Cell;
   end Next_State;
   
   procedure Print_State(s: in State) is
      cell_val : Cell;
      Rows : constant Positive := s.state_grid'Length(1);
      Cols : constant Positive := s.state_grid'Length(2);
   begin
      Put("--");
      for J in 1..Cols loop
         Put("--");
      end loop;
      Ada.Text_IO.New_Line;
      Put("|");
      for I in 1..Rows loop
         for J in 1..Cols loop
            cell_val := s.state_grid(I, J);
            case cell_val is 
            when Snake_Left | Snake_Right | Snake_Up | Snake_Down => 
               if s.snake_head = (I, J) then
                  case cell_val is 
                  when Snake_Left => Put("<<");
                  when Snake_Right => Put(">>");
                  when Snake_Up => Put(" A");
                  when Snake_Down => Put(" V");
                  when others => Put("  ");
                  end case;
               else
                  Put(" #");
               end if;
            when Empty => Put("  ");
            when Fruit => Put(" O");
            end case;
         end loop;
         Put("|");
         Ada.Text_IO.New_Line;
         Put("|");
      end loop;
      for J in 1..Cols loop
         Put("--");
      end loop;
      Put("|");
      Ada.Text_IO.New_Line;
   end Print_State;

   function Create_State(size: Natural) return State is
      s : State(size);
   begin
      s.snake_direction := Right;
      for I in 1..size loop
         for J in 1..size loop
            if (I-J) mod 4 = 0 and (I+J) mod 3 = 0 then
               s.state_grid(I, J) := Fruit;
            else
               s.state_grid(I, J) := Empty;
            end if;
         end loop;
      end loop;
      s.state_grid(size/2, size/2) := Snake_Right;
      s.state_grid(size/2, size/2-1) := Snake_Right;
      s.snake_head := (size/2, size/2);
      s.snake_tail := (size/2, size/2-1);
      return s;
   end Create_State;

   procedure Clear_Screen is
   begin
      put(ESC & "[2J");
   end;
   
   function Is_Snake(c: Cell) return Boolean is 
   begin
      case c is
         when Snake_Up | Snake_Down | Snake_Left | Snake_Right => return True;
         when others => return False;
      end case;
   end Is_Snake;
   
end snake;
