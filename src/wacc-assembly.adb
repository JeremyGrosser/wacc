pragma Style_Checks ("M120");
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Text_IO;

package body WACC.Assembly is

   procedure Generate
      (Tree : WACC.TACKY.Program_Node;
       Asm  : out WACC.Assembly.Program_Node)
   is
      function Convert_Operand
         (Tree : WACC.TACKY.Val_Node)
         return WACC.Assembly.Any_Operand_Node
      is
      begin
         case Tree.Typ is
            when WACC.TACKY.TA_Constant =>
               return new WACC.Assembly.Operand_Node'
                  (Typ => A_Imm,
                   Imm_Int => Tree.Int);
            when WACC.TACKY.TA_Var =>
               return new WACC.Assembly.Operand_Node'
                  (Typ => A_Pseudo,
                   Name => Tree.Name);
         end case;
      end Convert_Operand;

      function Convert_Unary_Operator
         (Tree : WACC.TACKY.Unary_Operator_Node)
         return WACC.Assembly.Any_Unary_Operator_Node
      is
      begin
         case Tree.Typ is
            when WACC.TACKY.TA_Complement =>
               return new WACC.Assembly.Unary_Operator_Node'
                  (Typ => WACC.Assembly.A_Not);
            when WACC.TACKY.TA_Negate =>
               return new WACC.Assembly.Unary_Operator_Node'
                  (Typ => WACC.Assembly.A_Neg);
         end case;
      end Convert_Unary_Operator;

      procedure Generate
         (Tree : WACC.TACKY.Instruction_Node;
          Node : in out WACC.Assembly.Instruction_Node_Vectors.Vector)
      is
         use WACC.Assembly.Instruction_Node_Vectors;
      begin
         case Tree.Typ is
            when WACC.TACKY.TA_Return =>
               Append (Node, new WACC.Assembly.Instruction_Node'
                  (Typ => WACC.Assembly.A_Mov,
                   Src => Convert_Operand (Tree.Val.all),
                   Dst => new WACC.Assembly.Operand_Node'
                     (Typ => WACC.Assembly.A_Reg,
                      Reg => new WACC.Assembly.Reg_Node'
                        (Typ => WACC.Assembly.A_AX))));
               Append (Node, new WACC.Assembly.Instruction_Node'
                  (Typ => WACC.Assembly.A_Ret));
            when WACC.TACKY.TA_Unary =>
               declare
                  Dst : constant Any_Operand_Node := Convert_Operand (Tree.Dst.all);
               begin
                  Append (Node, new WACC.Assembly.Instruction_Node'
                     (Typ => WACC.Assembly.A_Mov,
                      Src => Convert_Operand (Tree.Src.all),
                      Dst => Dst));
                  Append (Node, new WACC.Assembly.Instruction_Node'
                     (Typ => WACC.Assembly.A_Unary,
                      Unary_Operator => Convert_Unary_Operator (Tree.Unary_Operator.all),
                      Operand => Dst));
               end;
         end case;
      end Generate;

      procedure Generate
         (Tree : WACC.TACKY.Function_Definition_Node;
          Node : out WACC.Assembly.Function_Definition_Node)
      is
      begin
         Node.Name := Tree.Name;
         for TACKY_Insn of Tree.FBody loop
            Generate (TACKY_Insn.all, Node.Instructions);
         end loop;
      end Generate;

      package Stack_Maps is new Ada.Containers.Indefinite_Ordered_Maps
         (Key_Type     => String,
          Element_Type => Stack_Offset);
      Pseudo_Map : Stack_Maps.Map := Stack_Maps.Empty_Map;
      Next_Stack_Offset : Stack_Offset := Stack_Offset'Last;

      procedure Replace_Pseudo
         (Node : in out WACC.Assembly.Any_Operand_Node)
      is
      begin
         if Node.Typ = A_Pseudo then
            declare
               use Stack_Maps;
               Name  : constant String := To_String (Node.Name);
               PNode : Any_Operand_Node := Node;
            begin
               if not Contains (Pseudo_Map, Name) then
                  Insert (Pseudo_Map, Name, Next_Stack_Offset);
                  Next_Stack_Offset := Next_Stack_Offset - 4;
               end if;
               Node := new Operand_Node'
                  (Typ => A_Stack,
                   Stack_Int => Element (Pseudo_Map, Name));
               Free (PNode);
            end;
         end if;
      end Replace_Pseudo;

      procedure Replace_Pseudo
         (Node : in out WACC.Assembly.Instruction_Node)
      is
      begin
         case Node.Typ is
            when A_Mov =>
               Replace_Pseudo (Node.Src);
               Replace_Pseudo (Node.Dst);
            when A_Unary =>
               Replace_Pseudo (Node.Operand);
            when others =>
               null;
         end case;
      end Replace_Pseudo;

      procedure Replace_Pseudo
         (Node : in out WACC.Assembly.Function_Definition_Node)
      is
      begin
         for Insn of Node.Instructions loop
            Replace_Pseudo (Insn.all);
         end loop;
      end Replace_Pseudo;
   begin
      --  Pass 1: TACKY to Assembly
      Generate (Tree.Function_Definition, Asm.Function_Definition);
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "Pass 1:");
      Print (Asm);

      --  Pass 2: Replacing Pseudoregisters with Stack locations
      Replace_Pseudo (Asm.Function_Definition);
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "Pass 2:");
      Print (Asm);
   end Generate;

   procedure Print
      (Node : WACC.Assembly.Program_Node)
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
         (Node : WACC.Assembly.Reg_Node)
      is
      begin
         Write ("Reg");
         Indent;
         Write (Node.Typ'Image);
         Dedent;
      end Print;

      procedure Print
         (Node : WACC.Assembly.Operand_Node)
      is
      begin
         Write ("Operand");
         Indent;
         Write (Node.Typ'Image);
         case Node.Typ is
            when A_Imm =>
               Write ("Imm_Int = ");
               Write (Node.Imm_Int'Image);
            when A_Reg =>
               Write ("Reg = ");
               Print (Node.Reg.all);
            when A_Pseudo =>
               Write ("Name = ");
               Write (To_String (Node.Name));
            when A_Stack =>
               Write ("Stack_Int = ");
               Write (Node.Stack_Int'Image);
         end case;
         Dedent;
      end Print;

      procedure Print
         (Node : WACC.Assembly.Unary_Operator_Node)
      is
      begin
         Write ("Unary_Operator");
         Indent;
         Write (Node.Typ'Image);
         Dedent;
      end Print;

      procedure Print
         (Node : WACC.Assembly.Instruction_Node)
      is
      begin
         Write ("Instruction");
         Indent;
         Write (Node.Typ'Image);
         case Node.Typ is
            when A_Mov =>
               Write ("Src = ");
               Print (Node.Src.all);
               Write ("Dst = ");
               Print (Node.Dst.all);
            when A_Unary =>
               Write ("Unary_Operator = ");
               Print (Node.Unary_Operator.all);
               Write ("Operand = ");
               Print (Node.Operand.all);
            when A_Allocate_Stack =>
               Write ("Int = ");
               Write (Node.Int'Image);
            when A_Ret =>
               null;
         end case;
         Dedent;
      end Print;

      procedure Print
         (Node : WACC.Assembly.Function_Definition_Node)
      is
      begin
         Write ("Function_Definition");
         Indent;
         Write ("Name = " & To_String (Node.Name));
         Write ("Instructions = ");
         for Insn of Node.Instructions loop
            Print (Insn.all);
         end loop;
         Dedent;
      end Print;
   begin
      Write ("[Assembly]");
      Indent;
      Write ("Program_Node");
      Indent;
      Print (Node.Function_Definition);
      Dedent;
      Dedent;
   end Print;

end WACC.Assembly;
