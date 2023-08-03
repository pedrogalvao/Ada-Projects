with Ada.Text_IO;             use Ada.Text_IO;
with GNAT.Sockets;            use GNAT.Sockets;

with Ada.Streams;
use type Ada.Streams.Stream_Element_Count;


procedure Server is

   Server  : Socket_Type;
   Socket  : Socket_Type;
   Address : Sock_Addr_Type;
   Channel : Stream_Access; 

   Send   : String := (1 => ASCII.CR, 2 => ASCII.LF, 
                       3 => ASCII.CR, 4 => ASCII.LF);
   Offset : Ada.Streams.Stream_Element_Count;
   Data   : Ada.Streams.Stream_Element_Array (1 .. 256);

   Message : String(1..9);
begin
   Address.Addr := Inet_Addr("127.0.0.1");
   Address.Port := 5432;
   
   Create_Socket (Server);
   
   --  Allow reuse of local addresses.
   Set_Socket_Option
     (Server,
      Socket_Level,
      (Reuse_Address, True));
   
   Bind_Socket (Server, Address);
   
   --  A server marks a socket as willing to receive connect events.
   Listen_Socket (Server);
   
   --  Once a server calls Listen_Socket, incoming connects events
   --  can be accepted. The returned Socket is a new socket that
   --  represents the server side of the connection. Server remains
   --  available to receive further connections.
   
   Accept_Socket (Server, Socket, Address);
   Put_Line("Connection accepted");
   
   --  Return a stream associated to the connected socket.
   Channel := Stream (Socket);
   Put_Line("Stream channel created");
   
   --  Receive and print message from client Ping.
   loop
      Message := String'Input (Channel);
      Put_Line ("received message:");
      Put_Line (Message);
   
      --  Send same message to server Pong.
      String'Output (Channel, Message);
   end loop;
   
   Close_Socket (Server);
   Close_Socket (Socket);
   
end Server;
