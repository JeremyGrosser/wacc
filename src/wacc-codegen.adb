package body WACC.Codegen is

   function Generate
      (Tree : WACC.AST.Exp_Node)
      return WACC.Assembly.Any_Operand
   is
   begin
      case Tree.Typ is
         when WACC.AST.N_Constant =>
            return new WACC.Assembly.Operand'
               (Typ => WACC.Assembly.Imm,
                Int => Tree.Int);
         when WACC.AST.N_Unary =>
            return null; --  TODO
      end case;
   end Generate;

   procedure Generate
      (Tree : WACC.AST.Statement_Node;
       Asm  : in out WACC.Assembly.Instruction_Vectors.Vector)
   is
      use WACC.Assembly.Instruction_Vectors;
      Insn : WACC.Assembly.Any_Instruction;
   begin
      case Tree.Typ is
         when WACC.AST.N_Return =>
            Insn := new WACC.Assembly.Instruction'
               (Typ => WACC.Assembly.Mov,
                Src => Generate (Tree.Exp.all),
                Dst => new WACC.Assembly.Operand'
                  (Typ => WACC.Assembly.Register));
            Append (Asm, Insn);
            Insn := new WACC.Assembly.Instruction'
               (Typ => WACC.Assembly.Ret);
            Append (Asm, Insn);
      end case;
   end Generate;

   procedure Generate
      (Tree : WACC.AST.Function_Definition_Node;
       Asm   : out WACC.Assembly.Function_Definition_Node)
   is
   begin
      Asm.Name := Tree.Name;
      Generate (Tree.FBody.all, Asm.Instructions);
   end Generate;

   procedure Generate
      (Tree : WACC.AST.Program_Node;
       Asm  : out WACC.Assembly.Program_Node)
   is
   begin
      Generate (Tree.Function_Definition, Asm.Function_Definition);
   end Generate;

end WACC.Codegen;
