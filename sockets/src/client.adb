with Ada.Text_IO;             use Ada.Text_IO;
with GNAT.Sockets;            use GNAT.Sockets;

with Ada.Streams;
use type Ada.Streams.Stream_Element_Count;


procedure Client is

   Client  : Socket_Type;
   Address : Sock_Addr_Type;
   Channel : Stream_Access; 

   Send   : String := (1 => ASCII.CR, 2 => ASCII.LF, 
                       3 => ASCII.CR, 4 => ASCII.LF);
   Offset : Ada.Streams.Stream_Element_Count;
   Data   : Ada.Streams.Stream_Element_Array (1 .. 256);
   S: String(1..10);
begin

   GNAT.Sockets.Initialize;
   Create_Socket (Client);
   Address.Addr := Inet_Addr("127.0.0.1");
   Address.Port := 5432;

   Connect_Socket (Client, Address);
   Channel := Stream (Client);

   loop
      Put_Line("Type a message");
      Get(S);
      String'Write (Channel, S & Send);
      Put_Line("Message sent");
      Ada.Streams.Read (Channel.All, Data, Offset);
      exit when Offset = 0;
      for I in 1 .. Offset loop
         Ada.Text_IO.Put (Character'Val (Data (I)));
      end loop;
   end loop;

end Client;
