with Ada.Exceptions;
with Text_IO;

package body Byte_Buffer is

   overriding procedure Initialize(Object : in out Byte_Buffer_Object_Type) is

   begin

      Object.Data_Capacity := 0;
      Object.Data_Position := 0;

   end Initialize;


   overriding procedure Finalize(Object : in out Byte_Buffer_Object_Type) is

   begin

      null;

   end Finalize;

   procedure Allocate(Object : in out Byte_Buffer_Object_Type;
                      Capacity : in Ada.Streams.Stream_Element_Offset) is

      use type Ada.Streams.Stream_Element_Offset;

   begin

      if not Object.Allocated then
         Object.Data := new Ada.Streams.Stream_Element_Array(0 .. (Capacity - 1));
         Object.Data.all := (Others => 0);
         Object.Allocated := True;
         Object.Data_Capacity := Capacity;
      else
         raise Constraint_Error;
      end if;

   end Allocate;

   function Capacity(Object : in Byte_Buffer_Object_Type) return Interfaces.Integer_32 is

   begin

      return Interfaces.Integer_32(Object.Data_Capacity - 1);

   end Capacity;

   procedure Clear(Object : in out Byte_Buffer_Object_Type) is

   begin

      Object.Data_Capacity := 0;
      Object.Data_Position := 0;

   end Clear;

   procedure Put(Object : in out Byte_Buffer_Object_Type;
                 Index : in Ada.Streams.Stream_Element_Offset;
                 Value : Interfaces.Integer_8) is

   begin

      Object.Data(Index) := Ada.Streams.Stream_Element(Value);

   end Put;

   procedure Put_Short(Object : in out Byte_Buffer_Object_Type;
                 Index : in Ada.Streams.Stream_Element_Offset;
                       Value : Interfaces.Unsigned_16) is

      use type Interfaces.Unsigned_16;

   begin

      Object.Data(Index) := Ada.Streams.Stream_Element(16#00FF# and Value);
      Object.Data(Index+1) := Ada.Streams.Stream_Element(16#FF00# and Value);

   end Put_Short;

   procedure Put_Int(Object : in out Byte_Buffer_Object_Type;
                     Index : in Ada.Streams.Stream_Element_Offset;
                     Value : Interfaces.Unsigned_32) is

      use type Interfaces.Unsigned_32;

   begin

      Object.Data(Index) := Ada.Streams.Stream_Element(16#0000_00FF# and Value);
      Object.Data(Index+1) := Ada.Streams.Stream_Element(16#0000_FF00# and Value);
      Object.Data(Index+2) := Ada.Streams.Stream_Element(16#00FF_0000# and Value);
      Object.Data(Index+3) := Ada.Streams.Stream_Element(16#FF00_0000# and Value);

   end Put_Int;

   procedure Position(Object : in out Byte_Buffer_Object_Type;
                      Position : in Ada.Streams.Stream_Element_Offset) is

   begin

      Object.Data_Position := Position;

   end Position;

   procedure Put(Object : in out Byte_Buffer_Object_Type;
                 Src : in out Byte_Buffer_Object_Type) is

      Remaining : Ada.Streams.Stream_Element_Offset := Src.Data_Capacity - Src.Data_Position;

   begin

      pragma Compile_Time_Warning(True, "Runtime constraint error occurs here if initial size passed into Builder is less than final constructed size");
      Object.Data(Object.Data_Position .. Object.Data_Position+Remaining) := Src.Data(Src.Data_Position .. Src.Data_Position+Remaining);

   end Put;

   function Get_Short(Object : in out Byte_Buffer_Object_Type;
                      Index : in Ada.Streams.Stream_Element_Offset) return Interfaces.Unsigned_16 is

   begin

      return Interfaces.Shift_Left
        (Value  => Interfaces.Unsigned_16(Object.Data(Index)),
         Amount => 2) and Interfaces.Unsigned_16(Object.Data(Index+1));

   end Get_Short;

   procedure Print(Object : in out Byte_Buffer_Object_Type) is

      Index : Ada.Streams.Stream_Element_Offset := 0;

   begin

      Text_IO.Put_Line("Capacity---- " & Object.Capacity'Image);

      loop

         Text_IO.Put_Line(Object.Data(Index)'Img);
         Index := Index + 1;

         exit when Index >= Ada.Streams.Stream_Element_Offset(Object.Data_Capacity);
      end loop;

   end Print;

end Byte_Buffer;
