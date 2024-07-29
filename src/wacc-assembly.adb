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
      (N : Long_Integer)
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
      (Node : Operand_Node)
   is
   begin
      case Node.Typ is
         when A_Imm =>
            Write ("$");
            Write (Node.Int);
         when A_Register =>
            Write ("%eax");
      end case;
   end Print;

   procedure Print
      (Node : Instruction_Node)
   is
   begin
      case Node.Typ is
         when A_Mov =>
            Write ("movl ");
            Print (Node.Src.all);
            Write (", ");
            Print (Node.Dst.all);
         when A_Ret =>
            Write ("ret");
      end case;
   end Print;

   procedure Print
      (Node : Function_Definition_Node)
   is
      Name : constant String := To_String (Node.Name);
   begin
      Write ("    .globl ");
      Write (Name);
      New_Line;
      Write (Name);
      Write (':');
      Indent;
      for Insn of Node.Instructions loop
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
