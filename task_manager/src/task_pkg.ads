with Ada.Calendar; use Ada.Calendar;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package task_pkg is
   
   
   type Deadline_Kind_Type is (None,
                           Date,
                           DateTime);

   type Deadline (Kind : Deadline_Kind_Type) is record
      case Kind is
         when Date | DateTime =>
            t : Time;
         when None =>
            null;
      end case;
   end record;
   type Deadline_Access is access Deadline;
   
   type Task_Record is record
      title : unbounded_string;
      description : unbounded_string;
      task_deadline: Deadline_Access;
   end record;
   
   package Task_List is new
     Ada.Containers.Vectors
       (Index_Type   => Natural,
        Element_Type => Task_Record);
   
   function Compare_Task_Deadlines(task1, task2 : Task_Record) return Boolean;
   package Task_List_Sorting is
     new Task_List.Generic_Sorting(Compare_Task_Deadlines);
   
   procedure Print_Task(t: Task_Record; task_number : Positive);
   function Get_Task return Task_Record;
   
   type Command is (Add_Task, Delete_Task, View_Tasks, No_Command);
   function Parse_Command(cmd_str : Unbounded_String) return Command;
   
   procedure Add_New_Task(tl: in out Task_List.Vector);
   procedure Remove_Task(tl: in out Task_List.Vector);
   procedure Print_Task_List(tl: Task_List.Vector);
   procedure Order_By_Deadline(tl: in out Task_List.Vector);
end task_pkg;
