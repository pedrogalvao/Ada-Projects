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
   
   procedure Print_Deadline(d: Deadline) is
      Now_Year    : Year_Number;
      Now_Month   : Month_Number;
      Now_Day     : Day_Number;
      Now_Seconds : Day_Duration;
   begin
      case d.Kind is 
         when Date =>
            Split (d.t,
                   Now_Year,
                   Now_Month,
                   Now_Day,
                   Now_Seconds);

            Put (Day_Number'Image (Now_Day) & "/" & Month_Number'Image (Now_Month) & "/" & Year_Number'Image (Now_Year));
            Put_Line("");
         when DateTime =>
            Split (d.t,
                   Now_Year,
                   Now_Month,
                   Now_Day,
                   Now_Seconds);

            Put (Day_Number'Image (Now_Day) & "/" & Month_Number'Image (Now_Month) & "/" & Year_Number'Image (Now_Year));
            Put_Line("");
         when None =>
            Put_Line("");
      end case;
   end Print_Deadline;

   procedure Print_Task(t: Task_Record; task_number : Positive) is 
   begin
      for i in 0..50 loop
         Put("_");
      end loop;
      Put_Line("");
      Put(" " & Integer'Image(task_number) & " - " & To_String(t.title));
      for i in 0..(30 - To_String(t.title)'Length) loop
         Put(" ");
      end loop;
      Print_Deadline(t.task_deadline.all);
      if To_String(t.description)'Length > 0 then 
         for i in 0..50 loop
            Put("-");
         end loop;
         Put_Line("");
         Put_Line("  " & To_String(t.description));
      end if;
   end Print_Task;
   
   function Parse_Deadline(time_str : Unbounded_String) return Deadline_Access is
      t : Time;
      TZ : Time_Offset := UTC_Time_Offset;
      Day_Duration : Duration := 1.0 * 3600 * 24;
      Day_Str : Unbounded_String := To_Unbounded_String("");
      Month_Str : Unbounded_String := To_Unbounded_String("");
      Year_Str : Unbounded_String := To_Unbounded_String("");
      Day : Positive;
      Month : Positive;
      Year : Positive;
      Str_Index : Positive := 1;
      d : Deadline_Access;
   begin
         
      if time_str = To_Unbounded_String("") then 
         return new Deadline(None);
      elsif Case_Insensitive_Compare(time_str, To_Unbounded_String("now")) or else Case_Insensitive_Compare(time_str, To_Unbounded_String("today")) then
         t := Clock;
      elsif Case_Insensitive_Compare(time_str, To_Unbounded_String("tomorrow")) then
         t :=  Clock + Day_Duration;
      elsif Case_Insensitive_Compare(time_str, To_Unbounded_String("next week")) then
         t :=  Clock + 7 * Day_Duration;
      else
         while Str_Index < Length (time_str) loop
            if Element (time_str, Str_Index) = '/' or else Element (time_str, Str_Index) = '-' then
               exit;
            else
               Day_Str := Day_Str & Element (time_str, Str_Index);
            end if;
            Str_Index := Str_Index + 1;
         end loop;
         Day := Positive'Value(To_String(Day_Str));
         Put(To_String(Day_Str));
      
         while Str_Index < Length (time_str) loop
            Str_Index := Str_Index + 1;
            if Element (time_str, Str_Index) = '/' or else Element (time_str, Str_Index) = '-' then
               exit;
            else
               Month_Str := Month_Str & Element (time_str, Str_Index);
            end if;
         end loop;
         Month := Positive'Value(To_String(Month_Str));
         Put(To_String(Month_Str));
      
         while Str_Index < Length (time_str) loop
            Str_Index := Str_Index + 1;
            if Element (time_str, Str_Index) = '/' or else Element (time_str, Str_Index) = '-' then
               exit;
            else
               Year_Str := Year_Str & Element (time_str, Str_Index);
            end if;
         end loop;
         Year := Positive'Value(To_String(Year_Str));
         Put(To_String(Year_Str));
      
         t := Ada.Calendar.Formatting.Time_Of
           (Year        => Year,
            Month       => Month,
            Day         => Day,
            Hour        => 0,
            Minute      => 0,
            Second      => 1,
            Sub_Second  => 0.0,
            Leap_Second => False,
            Time_Zone   => TZ);
      
         --t := Ada.Calendar.Formatting.Value(To_String(time_str), TZ);
         
      end if;
      d := new Deadline(Date);
      d.all.t := t;
      return d;
   exception
      when E : CONSTRAINT_ERROR =>
         Put_Line ("Invalid date. Adding task without deadline");
         return new Deadline(None);
   end Parse_Deadline;
   
   function Get_Task return Task_Record is 
      t: Task_Record;
      task_title : Unbounded_String;
      task_description : Unbounded_String;
      task_deadline : Deadline_Access;
      TZ : Time_Offset := UTC_Time_Offset;
   begin
      while task_title = To_Unbounded_String("") loop
         Put("Task Title: ");
         task_title := To_Unbounded_String(Get_Line);
         if task_title = To_Unbounded_String("") then
            Put_Line("Title cannot be empty");
         end if;
      end loop;
      Put("Task Description: ");
      task_description := To_Unbounded_String(Get_Line);
      Put("Task Deadline: ");
      task_deadline := Parse_Deadline(To_Unbounded_String(Get_Line));
      t := (title => task_title, description => task_description, task_deadline => task_deadline);
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
      task_number : Positive := 1;
   begin
      if tl.Length > 0 then
         Put_Line("Task List:");
         for t of tl loop
            Print_Task(t, task_number);
            task_number := task_number + 1;
         end loop;
         for i in 0..50 loop
            Put("_");
         end loop;
         Put_Line("");
      else
         Put_Line("Task list is empty");
      end if;
   end Print_Task_List;
   
   procedure Remove_Task(tl: in out Task_List.Vector) is
      I : Integer;
      task_title : Unbounded_String;
      Task_Found : Boolean;
   begin
      Put ("Title or Number: ");
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
         I := Integer'Value(To_String(task_title)) - 1;
         if 0 <= I and then I < Integer(tl.Length) then
            tl.Delete(I);
         else
            Put_Line("Task not found");
         end if;
      end if;
      Put_Line("");
      exception
      when Constraint_Error =>
         Put_Line("Task not found");
         Put_Line("");
   end Remove_Task;
   
   function Compare_Task_Deadlines(task1, task2 : Task_Record) return Boolean is
   begin
      case task1.task_deadline.Kind is
         when None =>
            if task2.task_deadline.Kind = None then
               return task1.title < task2.title;
            else
               return false;
            end if;
         when Date | DateTime =>
            case task2.task_deadline.Kind is
            when None =>
               return true;
            when Date | DateTime =>
               return task1.task_deadline.all.t < task2.task_deadline.all.t;
            end case;
      end case;
   end Compare_Task_Deadlines;
   
   procedure Order_By_Deadline(tl: in out Task_List.Vector) is
   begin
      Task_List_Sorting.Sort
        (Container => tl);
   end Order_By_Deadline;
end task_pkg;
