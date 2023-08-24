with Ada.Containers.Indefinite_Hashed_Maps;

package state_machine is
   
   function Hash_Function (Key : Character) return Ada.Containers.Hash_Type;
   function Hash_Function (Key : Natural) return Ada.Containers.Hash_Type;
   
   package Edge_Map
   is new Ada.Containers.Indefinite_Hashed_Maps
     (Element_Type    => Natural,
      Key_Type        => Character,
      Hash            => Hash_Function,
      Equivalent_Keys => "=");
   
   type Node_Record is record
      Index : Natural;
      Adjacency_List : Edge_Map.Map;
   end record;
   
   function Create_Node return Node_Record;
   
   package Node_Map
   is new Ada.Containers.Indefinite_Hashed_Maps
     (Element_Type    => Node_Record,
      Key_Type        => Natural,
      Hash            => Hash_Function,
      Equivalent_Keys => "=");
   
   type Graph_Type is record
      Nodes : Node_Map.Map;
   end record;

   procedure Add_Node(Graph: out Graph_Type; New_Node: Node_Record);
   procedure Add_Edge(From_Node: in out Node_Record; To_Node: in out Node_Record; Transition: Character);
   procedure Add_Edge(Graph: in out Graph_Type; From_Node_Index: Natural; To_Node_Index: Natural; Transition: Character);
   
   type State_Machine_Record is record 
      State_Graph: Graph_Type;
      Current_State: Node_Record;
   end record;
   
   procedure Change_State(State_Machine: in out State_Machine_Record; transition: Character);
   procedure Process_String(State_Machine: in out State_Machine_Record; transitions: String);
   
end state_machine;
