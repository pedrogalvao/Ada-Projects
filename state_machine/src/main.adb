with Ada.Text_IO; use Ada.Text_IO;

with state_machine; use state_machine;

procedure Main is
   G: Graph_Type;
   N1 : Node_Record := Create_Node;
   N2 : Node_Record := Create_Node;
   N3 : Node_Record := Create_Node;
   SM : State_Machine_Record;
begin
   Add_Edge(N1, N2, 'a');
   Add_Edge(N2, N1, 'b');
   Add_Edge(N2, N3, 'a');
   Add_Node(G, N1);
   Add_Node(G, N2);
   Add_Node(G, N3);

   SM := (State_Graph => G, Current_State=> N1);

   Put(Natural'Image(SM.Current_State.Index));
   Process_String(SM, "a");
   Put(Natural'Image(SM.Current_State.Index));
   Process_String(SM, "b");
   Put(Natural'Image(SM.Current_State.Index));
   Process_String(SM, "a");
   Put(Natural'Image(SM.Current_State.Index));
   Process_String(SM, "a");
   Put(Natural'Image(SM.Current_State.Index));

end Main;
