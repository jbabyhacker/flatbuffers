with Ada.Finalization;
with Ada.Streams;
with Interfaces;

use type Ada.Streams.Stream_Element_Offset;
use type Interfaces.Integer_32;
use type Interfaces.Integer_16;
use type Interfaces.Unsigned_16;

package Byte_Buffer is

   type Byte_Buffer_Object_Type is new Ada.Finalization .Controlled with private;

   procedure Allocate(Object : in out Byte_Buffer_Object_Type;
                      Capacity : in Ada.Streams.Stream_Element_Offset);

   function Capacity(Object : in Byte_Buffer_Object_Type) return Interfaces.Integer_32;

   procedure Clear(Object : in out Byte_Buffer_Object_Type);

   procedure Put(Object : in out Byte_Buffer_Object_Type;
                 Index : in Ada.Streams.Stream_Element_Offset;
                 Value : Interfaces.Integer_8);

   procedure Put_Short(Object : in out Byte_Buffer_Object_Type;
                 Index : in Ada.Streams.Stream_Element_Offset;
                 Value : Interfaces.Unsigned_16);

   procedure Put_Int(Object : in out Byte_Buffer_Object_Type;
                     Index : in Ada.Streams.Stream_Element_Offset;
                     Value : Interfaces.Unsigned_32);

   procedure Position(Object : in out Byte_Buffer_Object_Type;
                      Position : in Ada.Streams.Stream_Element_Offset);

   procedure Put(Object : in out Byte_Buffer_Object_Type;
                 Src : in out Byte_Buffer_Object_Type);

   function Get_Short(Object : in out Byte_Buffer_Object_Type;
                      Index : in Ada.Streams.Stream_Element_Offset) return Interfaces.Unsigned_16;

   procedure Print(Object : in out Byte_Buffer_Object_Type);

private

   type Byte_Buffer_Object_Type is new Ada.Finalization.Controlled with record
      Data : access Ada.Streams.Stream_Element_Array := null;
      Data_Capacity : Ada.Streams.Stream_Element_Offset := 0;
      Data_Position : Ada.Streams.Stream_Element_Offset := 0;
      Allocated : Boolean := False;
   end record;

   overriding procedure Initialize(Object : in out Byte_Buffer_Object_Type);

   overriding procedure Finalize(Object : in out Byte_Buffer_Object_Type);

end Byte_Buffer;
