package WACC.IO
   with Elaborate_Body, SPARK_Mode => On
is
   type File_Type is private;

   function Is_Open
      (File : File_Type)
      return Boolean;

   procedure Open
      (File : in out File_Type;
       Filename : String)
   with Pre => not Is_Open (File),
        Post => Is_Open (File);

   procedure Close
      (File : in out File_Type)
   with Pre => Is_Open (File),
        Post => not Is_Open (File);

   function End_Of_File
      (File : File_Type)
       return Boolean
   with Pre => Is_Open (File);

   procedure Advance
      (File : in out File_Type)
   with Pre => Is_Open (File) and then not End_Of_File (File);

   function Peek
      (File : File_Type;
       Offset : Positive := 1)
       return Character
   with Pre => Is_Open (File) and then not End_Of_File (File);

   function Next
      (File : File_Type)
       return Character
   with Pre => Is_Open (File) and then not End_Of_File (File);

private

   type Any_String is access String;

   type File_Type is record
      Data   : Any_String := null;
      Index  : Natural := 0;
      Length : Natural := 0;
      Dirty  : Boolean := False;
   end record;

end WACC.IO;
