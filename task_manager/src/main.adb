with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar; use Ada.Calendar;
with task_pkg; use task_pkg;

procedure main is
   tl: Task_List.Vector;
   cmd_str : Unbounded_String;
   cmd : Command;
begin

   loop
      Put_Line("Options:");
      Put_Line(" - add");
      Put_Line(" - remove");
      Put_Line(" - view");

      cmd_str := To_Unbounded_String(Get_Line);
      cmd := Parse_Command(cmd_str);
      case cmd is
         when Add_Task =>
            Add_New_Task(tl);
            Order_By_Deadline(tl);
         when View_Tasks =>
            Print_Task_List(tl);
         when Delete_Task =>
            Remove_Task(tl);
         when No_Command => null;
      end case;
   end loop;

end main;
