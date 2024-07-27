pragma Warnings (Off, """System.Mmap"" is an internal GNAT unit");
pragma Style_Checks ("M120");
with System.Mmap;

package body WACC.IO is
   package TIO renames Ada.Text_IO;

   procedure Open
      (File : in out Reader;
       Filename : String)
   is
   begin
      File.Data := System.Mmap.Read_Whole_File (Filename);
      File.Index := File.Data'First;
   end Open;

   function Is_Open
      (File : Reader)
      return Boolean
   is (File.Index > 0);

   procedure Close
      (File : in out Reader)
   is
   begin
      GNAT.Strings.Free (File.Data);
      File.Index := 0;
   end Close;

   procedure Advance
      (File  : in out Reader;
       Count : Positive := 1)
   is
   begin
      File.Index := File.Index + Count;
   end Advance;

   function End_Of_File
      (File : Reader)
       return Boolean
   is (File.Index >= File.Data'Last);

   function Next
      (File : Reader)
       return Character
   is (if File.Index <= File.Data'Last then File.Data.all (File.Index) else ASCII.NUL);

   procedure Open
      (File : in out Writer;
       Filename : String)
   is
   begin
      TIO.Open
         (File => File.Output,
          Mode => TIO.Out_File,
          Name => Filename,
          Form => "8");
   end Open;

   function Is_Open
      (File : Writer)
      return Boolean
   is (TIO.Is_Open (File.Output));

   procedure Close
      (File : in out Writer)
   is
   begin
      TIO.Flush (File.Output);
      TIO.Close (File.Output);
   end Close;

   procedure Put
      (File : in out Writer;
       Ch   : Character)
   is
   begin
      TIO.Put (File.Output, Ch);
   end Put;

   procedure Put
      (File : in out Writer;
       Str  : String)
   is
   begin
      TIO.Put (File.Output, Str);
   end Put;

end WACC.IO;
