with Console_Server;
with Ada.Streams;
with Flatbuffer_Builder;
with Interfaces;

procedure Main is

   type My_Enum is (Procedure_1, Procedure_2);

--     type My_Procedure is access Procedure (Stream : Ada.Streams.Stream_Element_Array);

   type My_Mapping is array (My_Enum) of access Procedure (Stream : Ada.Streams.Stream_Element_Array);

   package Console is new Console_Server
     (In_Enum_Type       => My_Enum,
--        In_Subprogram_Type => My_Procedure,
      In_Mapping_Type    => My_Mapping);

   Builder : Flatbuffer_Builder.Flatbuffer_Builder_Object_Type(50);
   Struct : Interfaces.Integer_32;
   Obj : Interfaces.Integer_32;

begin

   Builder.Prep(4, 8);
   Builder.Put_Int(7);
   Builder.Put_Int(3);
   Struct := Builder.Offset;

   Builder.Start_Object(Num_Fields => 1);
   Builder.Add_Struct(voffset => 0,
                      x       => Struct,
                      d       => 0);
   Obj := Builder.End_Object;
   Builder.Finish(Obj);

--     Console.Initialize;

end Main;
