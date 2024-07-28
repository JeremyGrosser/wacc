with WACC.IO;

package body WACC.Assembly is

   Indent_Level : Natural := 0;
   File : WACC.IO.Writer;

   procedure Write
      (Str : String)
   is
   begin
      WACC.IO.Put (File, Str);
   end Write;

   procedure Write
      (Ch : Character)
   is
   begin
      WACC.IO.Put (File, Ch);
   end Write;

   procedure Write
      (N : Integer)
   is
   begin
      WACC.IO.Put (File, N);
   end Write;

   procedure New_Line is
   begin
      Write (ASCII.LF);
      for I in 1 .. Indent_Level loop
         Write ("    ");
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
            Write ("$");
            Write (This.Int);
         when Register =>
            Write ("%eax");
      end case;
   end Print;

   procedure Print
      (This : Instruction)
   is
   begin
      case This.Typ is
         when Mov =>
            Write ("mov ");
            Print (This.Src.all);
            Write (", ");
            Print (This.Dst.all);
         when Ret =>
            Write ("ret");
      end case;
   end Print;

   procedure Print
      (This : Function_Definition_Node)
   is
      Name : constant String := To_String (This.Name);
   begin
      Write (".globl ");
      Write (Name);
      New_Line;
      Write (Name);
      Write (':');
      Indent;
      for Insn of This.Instructions loop
         New_Line;
         Print (Insn.all);
      end loop;
      Dedent;
   end Print;

   procedure Print
      (Node     : Program_Node;
       Filename : String)
   is
   begin
      WACC.IO.Open (File, Filename);
      Print (Node.Function_Definition);
      New_Line;
      Write (".section .note.GNU-stack,"""",@progbits");
      New_Line;
      WACC.IO.Close (File);
   end Print;

end WACC.Assembly;
