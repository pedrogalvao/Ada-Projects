with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Calendar; use Ada.Calendar;
with task_pkg; use task_pkg;

procedure main is
   t: Task_Record;
   tl: Task_List.Vector;
   cmd_str : Unbounded_String;
   task_title : Unbounded_String;
   cmd : Command;
   -- Index : Task_List.Index;
   C : Task_List.Cursor;
   I : Integer;
begin

   Put_Line("Options:");
   Put_Line(" - add");
   Put_Line(" - remove");
   Put_Line(" - view");

   loop
      cmd_str := To_Unbounded_String(Get_Line);
      cmd := Parse_Command(cmd_str);
      case cmd is
         when Add_Task =>
            t := Get_Task;
            tl.Append(t);
         when View_Tasks =>
            for t of tl loop
               Print_Task(t);
            end loop;
         when Delete_Task =>
            Put_Line ("title:");
            task_title := To_Unbounded_String(Get_Line);
            I := 0;
            for task0 in tl.Iterate loop
               if tl.Element(I).title = task_title then
                  exit;
               end if;
               I := I + 1;
            end loop;
            tl.Delete(I);
         when No_Command => null;
      end case;
   end loop;

end main;
