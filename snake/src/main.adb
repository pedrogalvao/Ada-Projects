with snake; use snake;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Task_Identification;  use Ada.Task_Identification;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;

procedure Main is
   s : State := snake.Create_State(30);
   task Control_Direction;
   TaskID : Task_ID;

   task body Control_Direction is
      D : Character;
   begin
      while not s.game_over loop
         Get_Immediate (D);
         -- control snake direction using W, A, S, D or Z, Q, S, D
         case D is
            when 'z' | 'w' =>
               if s.snake_direction /= Down then
                  s.snake_direction := Up;
               end if;
            when 'q' | 'a' =>
               if s.snake_direction /= Right then
                  s.snake_direction := Left;
               end if;
            when 's' =>
               if s.snake_direction /= Up then
                  s.snake_direction := Down;
               end if;
            when 'd' =>
               if s.snake_direction /= Left then
                  s.snake_direction := Right;
               end if;
         when others => null; -- Handle other characters if needed
      end case;
   end loop;
end Control_Direction;
begin
   while not s.game_over loop
      Clear_Screen;
      Print_State(s);
      delay 0.1;
      Next_State(s);
   end loop;
   Put_Line("Game Over");
   Put("press enter to exit");
end Main;
