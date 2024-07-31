pragma Style_Checks ("M120");
with Ada.Text_IO;

package body WACC.TACKY is

   procedure Generate
      (Tree : WACC.AST.Program_Node;
       Node : out WACC.TACKY.Program_Node)
   is
      Tmp_Index : Natural := 0;

      function Make_Temporary
         return Identifier
      is
         S : Unbounded_String := To_Unbounded_String ("tmp.");
         N : constant String := Tmp_Index'Image;
      begin
         for Ch of N loop
            if Ch /= ' ' then
               Append (S, Ch);
            end if;
         end loop;
         Tmp_Index := Tmp_Index + 1;
         return S;
      end Make_Temporary;

      function Convert_Unop
         (Unop : WACC.AST.Unary_Operator_Node)
         return WACC.TACKY.Any_Unary_Operator_Node
      is
      begin
         case Unop.Typ is
            when WACC.AST.N_Complement =>
               return new WACC.TACKY.Unary_Operator_Node'(Typ => TA_Complement);
            when WACC.AST.N_Negate =>
               return new WACC.TACKY.Unary_Operator_Node'(Typ => TA_Negate);
         end case;
      end Convert_Unop;

      function Generate
         (Tree : WACC.AST.Exp_Node;
          Node : in out WACC.TACKY.Instruction_Node_Vectors.Vector)
          return Any_Val_Node
      is
         use WACC.TACKY.Instruction_Node_Vectors;
      begin
         case Tree.Typ is
            when WACC.AST.N_Constant =>
               return new WACC.TACKY.Val_Node'
                  (Typ => WACC.TACKY.TA_Constant,
                   Int => Tree.Int);
            when WACC.AST.N_Unary =>
               declare
                  Insn : constant WACC.TACKY.Any_Instruction_Node := new WACC.TACKY.Instruction_Node'
                     (Typ => WACC.TACKY.TA_Unary,
                      Dst => new Val_Node'
                        (Typ  => TA_Var,
                         Name => Make_Temporary),
                      Src => Generate (Tree.Exp.all, Node),
                      Unary_Operator => Convert_Unop (Tree.Unary_Operator.all));
               begin
                  Append (Node, Insn);
                  return Insn.Dst;
               end;
            when WACC.AST.N_Binary =>
               --  TODO
               raise Program_Error with "Unimplemented.";
         end case;
      end Generate;

      procedure Generate
         (Tree : WACC.AST.Statement_Node;
          Node : in out WACC.TACKY.Instruction_Node_Vectors.Vector)
      is
         use WACC.TACKY.Instruction_Node_Vectors;
      begin
         case Tree.Typ is
            when WACC.AST.N_Return =>
               Append (Node, new WACC.TACKY.Instruction_Node'
                  (Typ => WACC.TACKY.TA_Return,
                   Val => Generate (Tree.Exp.all, Node)));
         end case;
      end Generate;

      procedure Generate
         (Tree : WACC.AST.Function_Definition_Node;
          Node : out WACC.TACKY.Function_Definition_Node)
      is
      begin
         Node.Name := Tree.Name;
         Generate (Tree.FBody.all, Node.FBody);
      end Generate;
   begin
      Generate (Tree.Function_Definition, Node.Function_Definition);
   end Generate;

   procedure Print
      (Node : WACC.TACKY.Program_Node)
   is
      Indent_Level : Natural := 0;

      procedure Write
         (Str : String)
      is
      begin
         for I in 1 .. Indent_Level loop
            Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, ' ');
         end loop;
         Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, Str);
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
      end Write;

      procedure Indent is
      begin
         Indent_Level := Indent_Level + 1;
      end Indent;

      procedure Dedent is
      begin
         Indent_Level := Indent_Level - 1;
      end Dedent;

      procedure Print
         (Node : WACC.TACKY.Unary_Operator_Node)
      is
      begin
         Write ("Unary_Operator");
         Indent;
         Write (Node.Typ'Image);
         Dedent;
      end Print;

      procedure Print
         (Node : WACC.TACKY.Val_Node)
      is
      begin
         Write ("Val");
         Indent;
         Write (Node.Typ'Image);
         case Node.Typ is
            when TA_Constant =>
               Write (Node.Int'Image);
            when TA_Var =>
               Write ("Name = " & To_String (Node.Name));
         end case;
         Dedent;
      end Print;

      procedure Print
         (Node : WACC.TACKY.Instruction_Node)
      is
      begin
         Write ("Instruction");
         Indent;
         Write (Node.Typ'Image);
         case Node.Typ is
            when TA_Return =>
               Print (Node.Val.all);
            when TA_Unary =>
               Write ("Unary_Operator =");
               Print (Node.Unary_Operator.all);
               Write ("Dst = ");
               Print (Node.Dst.all);
               Write ("Src = ");
               Print (Node.Src.all);
         end case;
         Dedent;
      end Print;

      procedure Print
         (Node : WACC.TACKY.Function_Definition_Node)
      is
      begin
         Write ("Function_Definition");
         Indent;
         Write ("Name = " & To_String (Node.Name));
         Write ("FBody = ");
         for Insn of Node.FBody loop
            Print (Insn.all);
         end loop;
         Dedent;
      end Print;
   begin
      Write ("[TACKY]");
      Indent;
      Write ("Program_Node");
      Indent;
      Print (Node.Function_Definition);
      Dedent;
      Dedent;
   end Print;

end WACC.TACKY;
