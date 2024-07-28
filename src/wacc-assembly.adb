with Ada.Text_IO;

package body WACC.Assembly is

   Indent_Level : Natural := 0;

   procedure Log
      (Str : String)
   is
      use Ada.Text_IO;
   begin
      Put (Standard_Error, Str);
   end Log;

   procedure Log
      (N : Integer)
   is
      package Int_IO is new Ada.Text_IO.Integer_IO (Integer);
   begin
      Int_IO.Default_Width := 0;
      Int_IO.Put (Ada.Text_IO.Standard_Error, N);
   end Log;

   procedure New_Line is
      use Ada.Text_IO;
   begin
      New_Line (Standard_Error);
      for I in 1 .. Indent_Level loop
         Put (Standard_Error, "    ");
      end loop;
   end New_Line;

   procedure Indent is
   begin
      Indent_Level := Indent_Level + 1;
   end Indent;

   procedure Dedent is
   begin
      Indent_Level := Indent_Level - 1;
   end Dedent;

   procedure Print
      (This : Operand)
   is
   begin
      case This.Typ is
         when Imm =>
            Log ("#");
            Log (This.Int);
         when Register =>
            Log ("eax");
      end case;
   end Print;

   procedure Print
      (This : Instruction)
   is
   begin
      case This.Typ is
         when Mov =>
            Log ("mov ");
            Print (This.Src.all);
            Log (", ");
            Print (This.Dst.all);
         when Ret =>
            Log ("ret");
      end case;
   end Print;

   procedure Print
      (This : Function_Definition_Node)
   is
   begin
      Log (To_String (This.Name) & ":");
      Indent;
      for Insn of This.Instructions loop
         New_Line;
         Print (Insn.all);
      end loop;
      Dedent;
   end Print;

   procedure Print
      (Node : Program_Node)
   is
   begin
      Print (Node.Function_Definition);
   end Print;

end WACC.Assembly;
