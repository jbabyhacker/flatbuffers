with Text_IO;

package body Flatbuffer_Builder is

   overriding procedure Initialize(Object : in out Flatbuffer_Builder_Object_Type) is
      New_Initial_Size : Interfaces.Integer_32 := Interfaces.Integer_32(Object.Initial_Size);
   begin
      if New_Initial_Size <= 0 then
         New_Initial_Size := 1;
      end if;
      Object.space := New_Initial_Size;
      Object.bb.Allocate(Ada.Streams.Stream_Element_Offset(New_Initial_Size));
   end Initialize;

   overriding procedure Finalize(Object : in out Flatbuffer_Builder_Object_Type) is

   begin

      null;

   end Finalize;

   procedure Clear(Object : in out Flatbuffer_Builder_Object_Type) is

   begin

      Object.space := Interfaces.Integer_32(Object.bb.Capacity);
      Object.bb.Clear;
      Object.minalign := 1;
      loop

         exit when Object.vtable_in_use > 0;

         Object.vtable_in_use := Object.vtable_in_use - 1;

         Object.vtable(Object.vtable_in_use) := 0;

      end loop;

      Object.vtable_in_use := 0;
      Object.nested := False;
      Object.finished := False;
      Object.object_start := 0;
      Object.num_vtables := 0;
      Object.vector_num_elems := 0;

   end Clear;

   function Grow_Byte_Buffer(bb : in out Byte_Buffer.Byte_Buffer_Object_Type) return Byte_Buffer.Byte_Buffer_Object_Type is

      Old_Buf_Size : Interfaces.Unsigned_32 := Interfaces.Unsigned_32(bb.Capacity);
      New_Buf_Size : Interfaces.Unsigned_32 := (if Old_Buf_Size = 0 then 1 else Interfaces.Shift_Left(Value  => Old_Buf_Size,
                                                                                                      Amount => 1));
      nbb : Byte_Buffer.Byte_Buffer_Object_Type;

   begin
      Text_IO.Put_Line("Grow_Byte_Buffer");
      bb.Position(0);
      nbb.Allocate(Ada.Streams.Stream_Element_Offset(New_Buf_Size));
      nbb.Position(Ada.Streams.Stream_Element_Offset(New_Buf_Size - Old_Buf_Size));
      nbb.Put(bb);
      return nbb;

   end Grow_Byte_Buffer;


   function Offset(Object : in out Flatbuffer_Builder_Object_Type) return Interfaces.Integer_32 is

   begin

      return Interfaces.Integer_32(Object.bb.Capacity) - Object.space;

   end Offset;

   procedure Pad(Object : in out Flatbuffer_Builder_Object_Type;
                 Byte_Size : in Interfaces.Integer_32) is

   begin

      for Index in 0 .. Byte_Size-1 loop
         Object.space := Object.space - 1;
         Object.bb.Put(Index    => Ada.Streams.Stream_Element_Offset(Object.space),
                       Value => 0);
      end loop;

   end Pad;

--     function To_Twos_Complement(Num : in Interfaces.Unsigned_32) return Interfaces.Unsigned_32 is
--
--        type Bit_Array_Type is array (0 .. 31) of Boolean;
--        type Offset_Array_Type is array (0 .. 31) of Interfaces.Unsigned_32;
--
--        Offset_Array_One : Offset_Array_Type;
--        Offset_Array_Zero : Offset_Array_Type := (Others => (2^32) - 1);
--
--        Package To_Binary is new Text_IO.Modular_IO(Num => Interfaces.Unsigned_32);
--
--     begin
--
--        for Index in Offset_Array_One'Range loop
--           Offset_Array_One(Index) := 2 ** Index;
--           Offset_Array_Zero(Index) :=  Offset_Array_Zero(Index) xor Offset_Array_One(Index);
--
--        end loop;
--
--  --        if Num and Offset_Array(Index) = 1 then Num and
--
--  --        Num := (if Num and 16#1# = 1 then 0 else 1);
--
--     end To_Twos_Complement;

   procedure Prep(Object : in out Flatbuffer_Builder_Object_Type;
                  Size : in Interfaces.Integer_32;
                  Additional_Bytes : in Interfaces.Integer_32) is

      Align_Size : Interfaces.Integer_32;

   begin

      if Size > Object.minalign then
         Object.minalign := Size;
      end if;

      Text_IO.Put_Line("Capacity: " & Object.bb.Capacity'Img);
      Text_IO.Put_Line("Space: " & Object.space'Img);
      Text_IO.Put_Line("Additional_Bytes: " & Additional_Bytes'Img);
      Text_IO.Put_Line("Size: " & Size'Img);

      pragma Compile_Time_Warning(True, "Add two's complement");
      Align_Size := Interfaces.Integer_32(Interfaces.Unsigned_32(((Object.bb.Capacity - Object.space + Additional_Bytes)) + 1) and (Interfaces.Unsigned_32(Size) - 1));

      loop

         exit when Object.space >= Align_Size + Size + Additional_Bytes;

         declare

            Old_Buf_Size : Interfaces.Integer_32 := Interfaces.Integer_32(Object.bb.Capacity);

         begin

            Object.bb := Grow_Byte_Buffer(Object.bb);
            Object.space := Object.space + Object.bb.Capacity - Old_Buf_Size;
         end;

      end loop;

      Object.Pad(Align_Size);
   end Prep;

   procedure Put_Boolean(Object : in out Flatbuffer_Builder_Object_Type;
                         X : in Boolean) is

   begin
      Object.space := Object.space - Size_Of_Byte;

      Object.bb.Put
        (Index => Ada.Streams.Stream_Element_Offset(Object.space),
         Value  => (if X then 1 else 0));
   end Put_Boolean;

   procedure Put_Byte(Object : in out Flatbuffer_Builder_Object_Type;
                      X : in Interfaces.Integer_8) is

   begin
      Object.space := Object.space - Size_Of_Byte;

      Object.bb.Put
        (Index => Ada.Streams.Stream_Element_Offset(Object.space),
         Value  => x);
   end Put_Byte;

   procedure Put_Short(Object : in out Flatbuffer_Builder_Object_Type;
                     X : in Interfaces.Unsigned_16) is

   begin
      Object.space := Object.space - Size_Of_Short;

      Object.bb.Put_Short
        (Index => Ada.Streams.Stream_Element_Offset(Object.space),
         Value  => X);
   end Put_Short;

   procedure Put_Int(Object : in out Flatbuffer_Builder_Object_Type;
                     X : in Interfaces.Unsigned_32) is

   begin
      Object.space := Object.space - Size_Of_Int;

      Object.bb.Put_Int
        (Index => Ada.Streams.Stream_Element_Offset(Object.space),
         Value  => X);
   end Put_Int;

   procedure Add_Boolean(Object : in out Flatbuffer_Builder_Object_Type;
                         X : in Boolean) is

   begin

      Object.Prep(Size_Of_Byte, 0);
      Object.Put_Boolean(X);

   end Add_Boolean;

   procedure Add_Byte(Object : in out Flatbuffer_Builder_Object_Type;
                         X : in Interfaces.Integer_8) is

   begin

      Object.Prep(Size_Of_Byte, 0);
      Object.Put_Byte(X);

   end Add_Byte;

   procedure Add_Short(Object : in out Flatbuffer_Builder_Object_Type;
                         X : in Interfaces.Unsigned_16) is

   begin

      Object.Prep(Size_Of_Short, 0);
      Object.Put_Short(X);

   end Add_Short;

procedure Add_Int(Object : in out Flatbuffer_Builder_Object_Type;
                         X : in Interfaces.Unsigned_32) is

   begin

      Object.Prep(Size_Of_Int, 0);
      Object.Put_Int(X);

   end Add_Int;

   procedure Add_Offset(Object : in out Flatbuffer_Builder_Object_Type;
                        Off : in Interfaces.Integer_32) is

   begin
      Object.Prep
        (Size => Size_Of_Int,
         Additional_Bytes => 0);
      pragma Assert(off <= Object.Offset);
      Object.Put_Int(Interfaces.Unsigned_32(Object.Offset - off + Size_Of_Int));

   end Add_Offset;

   procedure Not_Nested(Object : in out Flatbuffer_Builder_Object_Type) is

   begin

      if Object.nested then
         raise Object_Serialization_Must_Not_Be_Nested;
      end if;

   end Not_Nested;

   procedure Is_Nested(Object : in out Flatbuffer_Builder_Object_Type;
                    obj : Interfaces.Integer_32) is

   begin

      if not (obj = Object.Offset) then
         raise Struct_Must_Be_Serialized_Inline;
      end if;

   end Is_Nested;

   procedure Start_Vector(Object : in out Flatbuffer_Builder_Object_Type;
                          elem_size : Interfaces.Integer_32;
                          num_elems : Interfaces.Integer_32;
                          alignment : Interfaces.Integer_32) is
   begin

      Object.Not_Nested;
      Object.vector_num_elems := num_elems;
      Object.Prep
        (Size => Size_Of_Int,
         Additional_Bytes => elem_size * num_elems);
      Object.Prep
        (Size => alignment,
         Additional_Bytes => elem_size * num_elems);
      Object.nested := True;

   end Start_Vector;

   function End_Vector(Object : in out Flatbuffer_Builder_Object_Type) return Interfaces.Integer_32 is

   begin

      if not Object.nested then
         raise End_Vector_Called_Without_Start_Vector;
      end if;

      Object.nested := False;
      Object.Put_Int(Interfaces.Unsigned_32(Object.vector_num_elems));
      return Object.Offset;

   end End_Vector;

   procedure Start_Object(Object : in out Flatbuffer_Builder_Object_Type;
                          Num_Fields : in Interfaces.Integer_32) is

   begin

      Object.Not_Nested;
      if Object.vtable = null or else Object.vtable'length < Num_Fields then
         Object.vtable := new vtable_array_type(0 .. Num_Fields);
      end if;
      Object.vtable_in_use := Num_Fields;
      Object.vtable(0 .. Object.vtable_in_use) := (Others => 0);
      Object.nested := True;
      Object.object_start := Object.Offset;
   end Start_Object;

   procedure Slot(Object : in out Flatbuffer_Builder_Object_Type;
                  voffset : Interfaces.Integer_32) is
   begin
      Object.vtable(voffset) := Object.Offset;
   end Slot;

   procedure Add_Boolean(Object : in out Flatbuffer_Builder_Object_Type;
                         o : Interfaces.Integer_32;
                         x : Boolean;
                         d : Boolean) is

   begin

      if Object.force_defaults or else x /= d then
         Object.Add_Boolean(x);
         Object.Slot(o);
      end if;

   end Add_Boolean;

   procedure Add_Byte(Object : in out Flatbuffer_Builder_Object_Type;
                         o : Interfaces.Integer_32;
                         x : Interfaces.Integer_8;
                         d : Interfaces.Integer_32) is

   begin

      if Object.force_defaults or else Interfaces.Integer_32(x) /= d then
         Object.Add_Byte(x);
         Object.Slot(o);
      end if;

   end Add_Byte;

   procedure Add_Struct(Object : in out Flatbuffer_Builder_Object_Type;
                        voffset : Interfaces.Integer_32;
                        x : Interfaces.Integer_32;
                        d : Interfaces.Integer_32) is



   begin

      if x /= d then
         Object.Is_Nested(obj => x);
         Object.Slot(voffset);
      end if;
   end Add_Struct;

   function End_Object(Object : in out Flatbuffer_Builder_Object_Type) return Interfaces.Integer_32 is

      vtableloc : Interfaces.Integer_32;
      i : Interfaces.Integer_32;
      trimmed_size : Interfaces.Integer_32;
      off : Interfaces.Unsigned_16;
      standard_fields : constant Interfaces.Integer_32 := 2;
      existing_vtable : Interfaces.Integer_32 := 0;
      j : Interfaces.Unsigned_16;

   begin

      if Object.vtable = null or (not Object.nested) then
         raise End_Object_Called_Without_Start_Object;
      end if;

      Object.Add_Int(0);

      vtableloc := Object.Offset;
      i := Object.vtable_in_use - 1;

      loop
         exit when i < 0 or Object.vtable(i) /= 0;
         i := i - 1;
      end loop;

      trimmed_size := i + 1;

      loop
         exit when i < 0;
         off := Interfaces.Unsigned_16( if Object.vtable(i) /= 0 then vtableloc - Object.vtable(i) else 0);
         Object.Add_Short(off);
         i := i - 1;
      end loop;

      Object.Add_Short(Interfaces.Unsigned_16(vtableloc - Object.object_start));
      Object.Add_Short(Interfaces.Unsigned_16((trimmed_size + standard_fields) * Size_Of_Short));

      i := 0;
      outer_loop:
      loop

         exit when i >= Object.num_vtables;

         declare
            vt1 : Interfaces.Integer_32 := Object.bb.Capacity - Object.vtables(i);
            vt2 : Interfaces.Integer_32 := Object.space;
            len : Interfaces.Unsigned_16 := Object.bb.Get_Short(Ada.Streams.Stream_Element_Offset(vt1));
         begin
            if len = Object.bb.Get_Short(Ada.Streams.Stream_Element_Offset(vt2)) then
               j := Size_Of_Short;
               loop
                  exit when j >= len;
                  if Object.bb.Get_Short(Ada.Streams.Stream_Element_Offset(vt1 + Interfaces.Integer_32(j))) /= Object.bb.Get_Short(Ada.Streams.Stream_Element_Offset(vt2 + Interfaces.Integer_32(j))) then
                     goto outer_goto;
                  end if;
                  j := j + Size_Of_Short;
               end loop;

               existing_vtable := Object.vtables(i);
               exit outer_loop;
            end if;
         end;
         <<outer_goto>>
         i := i + 1;
      end loop outer_loop;

      if existing_vtable /= 0 then
         Object.space := Object.bb.Capacity - vtableloc;
         Object.bb.Put_Int(Index => Ada.Streams.Stream_Element_Offset(Object.space),
                           Value => Interfaces.Unsigned_32(existing_vtable - vtableloc));
      else
         if Object.num_vtables = Object.vtables'length then
            Object.vtables(Object.num_vtables * 2 .. Object.vtables'last) := (others => 0);
         end if;
         Object.vtables(Object.num_vtables) := Object.Offset;
         Object.num_vtables := Object.num_vtables + 1;
         Object.bb.Put_Int( Ada.Streams.Stream_Element_Offset(Object.bb.Capacity - vtableloc), Interfaces.Unsigned_32(Object.Offset - vtableloc));
      end if;

      Object.nested := false;

      return vtableloc;

   end End_Object;

   procedure Finish(Object : in out Flatbuffer_Builder_Object_Type;
                    Root_Table : in Interfaces.Integer_32;
                    Size_Prefix : Boolean) is

   begin

      Object.Prep(Object.minalign, Size_Of_Int + (if Size_Prefix then Size_Of_Int else 0));
      Object.Add_Offset(Root_Table);
      if Size_Prefix then
         Object.Add_Int(Interfaces.Unsigned_32(Object.bb.Capacity - Object.space));
      end if;

      Object.bb.Position(Ada.Streams.Stream_Element_Offset(Object.space));
      Object.finished := True;

   end Finish;

   procedure Finish(Object : in out Flatbuffer_Builder_Object_Type;
                    Root_Table : in Interfaces.Integer_32) is

   begin

      Object.Finish(Root_Table, False);
      Object.bb.Print;

   end Finish;

   procedure Finish_Size_Prefixed(Object : in out Flatbuffer_Builder_Object_Type;
                                  Root_Table : Interfaces.Integer_32) is

   begin

      Object.Finish(Root_Table, True);

   end Finish_Size_Prefixed;

   procedure Finished(Object : in out Flatbuffer_Builder_Object_Type) is

   begin

      if not Object.finished then
         raise You_Can_Only_Access_The_Serialized_Buffer_After_It_Has_Been_Finished;
      end if;

   end Finished;

--     function Sized_Byte_Array(Object : in out Flatbuffer_Builder_Object_Type;
--                               Start : in Interfaces.Integer_32;
--                               Length : in Interfaces.Integer_32) return Ada.Streams.Stream_Element_Array is
--
--     begin
--
--        Object.Finished;
--        Object.bb.Position(start);
--
--
--     end Sized_Byte_Array;
--
--     function Sized_Byte_Array(Object : in out Flatbuffer_Builder_Object_Type) return Ada.Streams.Stream_Element_Array is
--
--     begin
--
--        return Object.Size_Byte_Array(Object.space, Object.bb.Capacity - Object.space);
--
--     end Sized_Byte_Array;


end Flatbuffer_Builder;
