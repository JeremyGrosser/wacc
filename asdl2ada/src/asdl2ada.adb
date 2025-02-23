pragma Ada_2022;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with ASDL.Parser;
with ASDL.Codegen;

procedure ASDL2Ada is
   --  definitions = {typ_id "=" type}
   --  type = {sum_type | product_type}
   --  product_type = fields
   --  sum_type = constructor {"|" constructor}
   --             ["attributes" fields]
   --  constructor = con_id [fields]
   --  fields = "(" {field ","} field ")"
   --  field = typ_id ["?" | "*"] [id]
   --
   --  upper = "A" .. "Z"
   --  lower = "a" .. "z"
   --  alpha = "_" | upper | lower
   --  alpha_num = alpha | "0" .. "9"
   --  typ_id = lower {alpha_num}
   --  con_id = upper {alpha_num}
   --  id = typ_id | con_id

begin
   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
         "Usage: asdl2ada <filename>");
      Ada.Command_Line.Set_Exit_Status (1);
      return;
   end if;

   declare
      Filename : constant String := Ada.Command_Line.Argument (1);
      Length : constant Natural := Natural (Ada.Directories.Size (Filename));
      Text : String (1 .. Length);
      File : Ada.Text_IO.File_Type;
      Input : Ada.Text_IO.Text_Streams.Stream_Access;

      Module : ASDL.Parser.ASDL_Module;
   begin
      Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Filename);
      Input := Ada.Text_IO.Text_Streams.Stream (File);
      String'Read (Input, Text);
      Ada.Text_IO.Close (File);

      ASDL.Parser.Parse (Text, Module);
      ASDL.Codegen.Generate (Module);
   end;
end ASDL2Ada;
