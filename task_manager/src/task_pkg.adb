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
   
   procedure Print_Time(t: Time) is
      Now_Year    : Year_Number;
      Now_Month   : Month_Number;
      Now_Day     : Day_Number;
      Now_Seconds : Day_Duration;
   begin
      Split (t,
             Now_Year,
             Now_Month,
             Now_Day,
             Now_Seconds);

      Put (Day_Number'Image (Now_Day) & "/" & Month_Number'Image (Now_Month) & "/" & Year_Number'Image (Now_Year));
      Put_Line("");
   end Print_Time;

   procedure Print_Task(t: Task_Record) is 
   begin
      for i in 0..50 loop
         Put("_");
      end loop;
      Put_Line("");
      Put("  " & To_String(t.title));
      for i in 0..(35 - To_String(t.title)'Length) loop
         Put(" ");
      end loop;
      Print_Time(t.deadline);
      if To_String(t.description)'Length > 0 then 
         for i in 0..50 loop
            Put("-");
         end loop;
         Put_Line("");
         Put_Line("  " & To_String(t.description));
      end if;
   end Print_Task;
   
   function Parse_Time(time_str : Unbounded_String) return Time is
      t : Time;
      TZ : Time_Offset := UTC_Time_Offset;
      Day_Duration : Duration := 1.0 * 3600 * 24;
   begin
      if Case_Insensitive_Compare(time_str, To_Unbounded_String("now")) then
         return Clock;
      elsif Case_Insensitive_Compare(time_str, To_Unbounded_String("tomorrow")) then
         return Clock + Day_Duration;
      end if;
      t := Ada.Calendar.Formatting.Value(To_String(time_str), TZ);
      return t;
   end Parse_Time;
   
   function Get_Task return Task_Record is 
      t: Task_Record;
      task_title : Unbounded_String;
      task_description : Unbounded_String;
      task_deadline : Time;
      TZ : Time_Offset := UTC_Time_Offset;
   begin
      Put("Task Title: ");
      task_title := To_Unbounded_String(Get_Line);
      Put("Task Description: ");
      task_description := To_Unbounded_String(Get_Line);
      Put("Task Deadline: ");
      task_deadline := Parse_Time(To_Unbounded_String(Get_Line));
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
   
   procedure Add_New_Task(tl: in out Task_List.Vector) is
      t : Task_Record;
   begin
      t := Get_Task;
      tl.Append(t);
      Put_Line("New task added");
      Put_Line("");
   end Add_New_Task;
   
   procedure Print_Task_List(tl: Task_List.Vector) is
   begin
      if tl.Length > 0 then
         Put_Line("Task List:");
         for t of tl loop
            Print_Task(t);
         end loop;
         for i in 0..50 loop
            Put("_");
         end loop;
         Put_Line("");
      else
         Put("Task list is empty");
      end if;
   end Print_Task_List;
   
   procedure Remove_Task(tl: in out Task_List.Vector) is
      I : Integer;
      task_title : Unbounded_String;
      Task_Found : Boolean;
   begin
      Put ("Title:");
      task_title := To_Unbounded_String(Get_Line);
      I := 0;
      Task_Found := False;
      for task0 in tl.Iterate loop
         if Case_Insensitive_Compare(tl.Element(I).title, task_title) then
            Task_Found := True;
            exit;
         end if;
         I := I + 1;
      end loop;
      if Task_Found then
         tl.Delete(I);
         Put_Line("Task removed");
      else
         Put_Line("Task not found");
      end if;
      Put_Line("");
   end Remove_Task;
end task_pkg;
