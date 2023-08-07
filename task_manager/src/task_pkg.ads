with Ada.Calendar; use Ada.Calendar;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package task_pkg is
   
   type Task_Record is record
      title : unbounded_string;
      description : unbounded_string;
      deadline: Time;
   end record;
   
   package Task_List is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => Task_Record);
   
   procedure Print_Task(t: Task_Record);
   function Get_Task return Task_Record;
   
   type Command is (Add_Task, Delete_Task, View_Tasks, No_Command);
   function Parse_Command(cmd_str : Unbounded_String) return Command;
   
end task_pkg;
