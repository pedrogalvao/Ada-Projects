package body state_machine is
   function Hash_Function (Key : Character) return Ada.Containers.Hash_Type is
      Hash_Value : Ada.Containers.Hash_Type := 0;
   begin
      Hash_Value := Ada.Containers.Hash_Type (Character'Pos(Key));
      return Hash_Value;
   end Hash_Function;
   
   function Hash_Function (Key : Natural) return Ada.Containers.Hash_Type is
      Hash_Value : Ada.Containers.Hash_Type := 0;
   begin
      Hash_Value := Ada.Containers.Hash_Type (Key);
      return Hash_Value;
   end Hash_Function;
   
   Current_Node_Index : Natural := 0;
   function Create_Node return Node_Record is
      ad_list : Edge_Map.Map;
      N : Node_Record;
   begin
      Current_Node_Index := Current_Node_Index + 1;
      N := (Index=>Current_Node_Index, Adjacency_List=>ad_list);
      return N;
   end Create_Node;

   procedure Add_Edge(From_Node: in out Node_Record; To_Node: in out Node_Record; Transition: Character) is
   begin
      From_Node.Adjacency_List.Insert(Transition, To_Node.Index);
   end Add_Edge;
   
   procedure Add_Edge(Graph: in out Graph_Type; From_Node_Index: Natural; To_Node_Index: Natural; Transition: Character) is
      From_Node : Node_Record;
   begin
      From_Node := Graph.Nodes.Element(From_Node_Index);
      From_Node.Adjacency_List.Insert(Transition, To_Node_Index);
   end Add_Edge;
   
   procedure Add_Node(Graph: out Graph_Type; New_Node: Node_Record) is 
   begin
      Graph.Nodes.Insert(New_Node.Index, New_Node);
   end Add_Node;
   
   procedure Change_State(State_Machine: in out State_Machine_Record; transition: Character) is
      Current_State_Index : Natural;
   begin 
      if State_Machine.Current_State.Adjacency_List.Contains(transition) then
         Current_State_Index := State_Machine.Current_State.Adjacency_List.Element(transition);
         State_Machine.Current_State := State_Machine.State_Graph.Nodes.Element(Current_State_Index);
      end if;
   end Change_State;
   
   procedure Process_String(State_Machine: in out State_Machine_Record; transitions: String) is
   begin
      for c of transitions loop
         Change_State(State_Machine, c);
      end loop;
   end Process_String;
   
end state_machine;
