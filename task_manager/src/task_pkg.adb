with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps;
with Ada.Strings.Maps.Constants;

with Ada.Calendar.Formatting;
use  Ada.Calendar.Formatting;

with Ada.Calendar.Time_Zones;
use  Ada.Calendar.Time_Zones;

package body task_pkg is
   
   function To_Lower_Case(Str : in Ada.Strings.Unbounded.Unbounded_String) return Ada.Strings.Unbounded.Unbounded_String is
      Temp : constant String := Ada.Strings.Unbounded.To_String(Str);
   begin
      return 
        Ada.Strings.Unbounded.Translate(Str, Ada.Strings.Maps.Constants.Lower_Case_Map);
   end To_Lower_Case;
   
   function Case_Insensitive_Compare(A, B : in Unbounded_String) return Boolean is
   begin
      return To_Lower_Case(A) = To_Lower_Case(B);
   end Case_Insensitive_Compare;
   

   procedure Print_Task(t: Task_Record) is 
   begin
      for i in 0..20 loop
         Put("_");
      end loop;
      Put_Line("");
      Put_Line(To_String(t.title));
      Put_Line(To_String(t.description));
   end Print_Task;
   
   function Get_Task return Task_Record is 
      t: Task_Record;
      task_title : Unbounded_String;
      task_description : Unbounded_String;
      task_deadline : Time;
      TZ : Time_Offset := UTC_Time_Offset;
   begin
      Put_Line("Task Title:");
      task_title := To_Unbounded_String(Get_Line);
      Put_Line("Task Description:");
      task_description := To_Unbounded_String(Get_Line);
      Put_Line("Task Deadline:");
      task_deadline := Ada.Calendar.Formatting.Value(Get_Line, TZ);
      t := (title => task_title, description => task_description, Deadline => task_deadline);
      return t;
   end Get_Task;
   
   function Parse_Command(cmd_str : Unbounded_String) return Command is
   begin
      if Case_Insensitive_Compare(cmd_str, To_Unbounded_String("add")) then
         return Add_Task;
      elsif Case_Insensitive_Compare(cmd_str, To_Unbounded_String("view")) then
         return View_Tasks;
      elsif Case_Insensitive_Compare(cmd_str, To_Unbounded_String("remove")) then
         return Delete_Task;
      end if;
      return No_Command;
   end Parse_Command;
end task_pkg;
