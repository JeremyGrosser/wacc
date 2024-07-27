private with Ada.Text_IO;

package WACC.IO
   with Elaborate_Body, SPARK_Mode => On
is
   type Reader is private;

   function Is_Open
      (File : Reader)
      return Boolean;

   procedure Open
      (File : in out Reader;
       Filename : String)
   with Pre => not Is_Open (File),
        Post => Is_Open (File);

   procedure Close
      (File : in out Reader)
   with Pre => Is_Open (File),
        Post => not Is_Open (File);

   function End_Of_File
      (File : Reader)
       return Boolean
   with Pre => Is_Open (File);

   procedure Advance
      (File  : in out Reader;
       Count : Positive := 1)
   with Pre => Is_Open (File) and then not End_Of_File (File);

   function Peek
      (File   : Reader;
       Offset : Positive := 1)
       return Character
   with Pre => Is_Open (File);

   function Lookahead
      (File  : Reader;
       Count : Positive)
       return String
   with Pre => Is_Open (File);

   function Next
      (File : Reader)
       return Character
   with Pre => Is_Open (File);

   type Writer is limited private;

   function Is_Open
      (File : Writer)
      return Boolean;

   procedure Open
      (File : in out Writer;
       Filename : String)
   with Pre => not Is_Open (File),
        Post => Is_Open (File);

   procedure Put
      (File : in out Writer;
       Ch   : Character)
   with Pre => Is_Open (File);

   procedure Put
      (File : in out Writer;
       Str  : String)
   with Pre => Is_Open (File);

   procedure Close
      (File : in out Writer)
   with Pre => Is_Open (File),
        Post => not Is_Open (File);

private

   type Any_String is access String;

   type Reader is record
      Data   : Any_String := null;
      Index  : Natural := 0;
      Length : Natural := 0;
      Dirty  : Boolean := False;
   end record;

   type Writer is limited record
      Output : Ada.Text_IO.File_Type;
   end record;

end WACC.IO;
