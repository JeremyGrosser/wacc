with Ada.Text_IO;
with Ada.Directories;
with Ada.Unchecked_Deallocation;

package body WACC.IO is
   package TIO renames Ada.Text_IO;

   procedure Free is new Ada.Unchecked_Deallocation
      (String, Any_String);

   procedure Open
      (File : in out File_Type;
       Filename : String)
   is
      Input : TIO.File_Type;
   begin
      File.Length := Natural (Ada.Directories.Size (Filename));
      File.Data := new String (1 .. File.Length);
      File.Index := File.Data'First;
      TIO.Open
         (File => Input,
          Mode => TIO.In_File,
          Name => Filename,
          Form => "8"); --  all inputs are assumed UTF-8
   end Open;

   function Is_Open
      (File : File_Type)
      return Boolean
   is (File.Index > 0);

   procedure Close
      (File : in out File_Type)
   is
   begin
      Free (File.Data);
      File.Length := 0;
      File.Index := 0;
   end Close;

   procedure Advance
      (File : in out File_Type)
   is
   begin
      File.Index := File.Index + 1;
   end Advance;

   function End_Of_File
      (File : File_Type)
       return Boolean
   is (File.Index >= File.Data'Last);

   function Peek
      (File : File_Type;
       Offset : Positive := 1)
       return Character
   is (File.Data (File.Index + Offset));

   function Next
      (File : File_Type)
       return Character
   is (File.Data (File.Index));

end WACC.IO;
