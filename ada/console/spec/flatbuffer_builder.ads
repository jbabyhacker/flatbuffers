with Ada.Finalization;
with Ada.Streams;
with Ada.Containers;
with Ada.Containers.Bounded_Vectors;
with Interfaces;
with Byte_Buffer;

use type Ada.Containers.Count_Type;
use type Interfaces.Integer_32;
use type Ada.Streams.Stream_Element_Offset;
use type Interfaces.Unsigned_32;
use type Interfaces.Integer_16;
use type Interfaces.Unsigned_16;

package Flatbuffer_Builder is

   type Flatbuffer_Builder_Object_Type(Initial_Size : Ada.Streams.Stream_Element_Offset) is new Ada.Finalization .Controlled with private;

   procedure Start_Object(Object : in out Flatbuffer_Builder_Object_Type;
                          Num_Fields : in Interfaces.Integer_32);

   function End_Object(Object : in out Flatbuffer_Builder_Object_Type) return Interfaces.Integer_32;

   procedure Add_Struct(Object : in out Flatbuffer_Builder_Object_Type;
                        voffset : Interfaces.Integer_32;
                        x : Interfaces.Integer_32;
                        d : Interfaces.Integer_32);

   procedure Prep(Object : in out Flatbuffer_Builder_Object_Type;
                  Size : in Interfaces.Integer_32;
                  Additional_Bytes : in Interfaces.Integer_32);

   procedure Put_Int(Object : in out Flatbuffer_Builder_Object_Type;
                     X : in Interfaces.Unsigned_32);

   function Offset(Object : in out Flatbuffer_Builder_Object_Type) return Interfaces.Integer_32;

   procedure Finish(Object : in out Flatbuffer_Builder_Object_Type;
                    Root_Table : in Interfaces.Integer_32);

private

   Object_Serialization_Must_Not_Be_Nested : Exception;
   Struct_Must_Be_Serialized_Inline : Exception;
   End_Vector_Called_Without_Start_Vector : Exception;
   End_Object_Called_Without_Start_Object : Exception;
   You_Can_Only_Access_The_Serialized_Buffer_After_It_Has_Been_Finished : Exception;

   Size_Of_Byte : constant := 1;
   Size_Of_Short : constant := 2;
   Size_Of_Int : constant := 4;

   use type Interfaces.Unsigned_8;

--     subtype Index_Range_Type is Ada.Containers.Count_Type range 0 .. 1000;
--
--     package Vector_Instantiation is new Ada.Containers.Bounded_Vectors
--       (Index_Type   => Index_Range_Type,
--        Element_Type => Interfaces.Unsigned_8);

   type vtable_array_type is array (Interfaces.Integer_32 range <>) of Interfaces.Integer_32;

   type Flatbuffer_Builder_Object_Type(Initial_Size : Ada.Streams.Stream_Element_Offset) is new Ada.Finalization .Controlled with record
      bb : Byte_Buffer.Byte_Buffer_Object_Type;
      space : Interfaces.Integer_32 := 0;
      minalign : Interfaces.Integer_32 := 1;
      vtable : access vtable_array_type := null;
      vtable_in_use : Interfaces.Integer_32 := 0;
      nested : Boolean := False;
      finished : Boolean := False;
      object_start : Interfaces.Integer_32 := 0;
      vtables : vtable_array_type(0 .. 15);
      num_vtables : Interfaces.Integer_32 := 0;
      vector_num_elems : Interfaces.Integer_32 := 0;
      force_defaults : Boolean := False;
      dst : Byte_Buffer.Byte_Buffer_Object_Type;
   end record;

   overriding procedure Initialize(Object : in out Flatbuffer_Builder_Object_Type);

   overriding procedure Finalize(Object : in out Flatbuffer_Builder_Object_Type);

end Flatbuffer_Builder;
