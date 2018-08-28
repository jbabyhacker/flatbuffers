with Ada.Streams;
with ZMQ.Sockets;
with ZMQ.Contexts;

generic

   type In_Enum_Type is (<>);
--     type In_Subprogram_Type is access Procedure (Stream : Ada.Streams.Stream_Element_Array);
   type In_Mapping_Type is array (In_Enum_Type) of access procedure (Stream : Ada.Streams.Stream_Element_Array);

--     Have this accept a procedure that calls the callback. Each callback has to be called in a different way.

--     type Out_Enum_Type is (<>);
--     type Out_Subprogram_Type is private;
--     type Out_Mapping_Type is array (Out_Enum_Type) of Out_Subprogram_Type;
--
--     type In_Out_Enum_Type is (<>);
--     type In_Out_Subprogram_Type is private;
--     type In_Out_Mapping_Type is array (In_Out_Enum_Type) of In_Out_Subprogram_Type;

package Console_Server is

   procedure Initialize;

--     procedure Register(Key : in In_Enum_Type;
--                        Callback : access procedure (Stream : Ada.Streams.Stream_Element_Array));

private

   Context : ZMQ.Contexts.Context;
   Socket : ZMQ.Sockets.Socket;


   Callbacks : In_Mapping_Type := (Others => Null);

end Console_Server;
