pragma Style_Checks ("M120");

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
   begin
      Generate (Tree.Function_Definition, Asm.Function_Definition);
   end Generate;

   procedure Print
      (Node : WACC.Assembly.Program_Node;
       Filename : String)
   is
   begin
      null;
   end Print;

end WACC.Assembly;
