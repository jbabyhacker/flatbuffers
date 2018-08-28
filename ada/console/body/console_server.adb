with Text_IO;
with ZMQ.Messages;

package body Console_Server is

   procedure Initialize is

      My_Message : ZMQ.Messages.Message;
      My_Data : access Ada.Streams.Stream_Element_Array := new Ada.Streams.Stream_Element_Array(1 .. 14);

   begin

      Socket.Initialize (Context, ZMQ.Sockets.REP);
      Socket.Bind("tcp://*:5555");
      My_Message.Initialize(My_Data.all);

      loop
         Text_IO.Put_Line("Waiting...");

         My_Data.all := (others => 0);

--           Text_IO.Put_Line(Socket.Recv);
         Socket.Recv(My_Message);

         declare
           D : Ada.Streams.Stream_Element_Array := My_Message.GetData;
         begin

            Text_IO.Put_Line("Length: " & D'Length'Img);

         For Index in D'Range loop
            Text_IO.Put_Line(D(Index)'Img);
         end loop;

         end;




         Text_IO.Put_Line("Got");

         Socket.Send("Cool");

      end loop;


   end Initialize;

   --     procedure Register(Key : in In_Enum_Type;
   --                        Callback : access procedure (Stream : Ada.Streams.Stream_Element_Array)) is
   --
   --     begin
   --
   --        Callbacks(Key) := Callback'Unrestricted_Access;
   --
   --     end Register;

end Console_Server;
