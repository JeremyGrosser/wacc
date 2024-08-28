pragma Style_Checks ("M120");
with Ada.Text_IO;

package body WACC.TACKY is

   procedure Generate
      (Tree : WACC.AST.Program_Node;
       Node : out WACC.TACKY.Program_Node)
   is
      procedure Generate
         (Tree : WACC.AST.Declaration_Node;
          Node : in out WACC.TACKY.Instruction_Node_Vectors.Vector);
      procedure Generate
         (Tree : WACC.AST.Variable_Declaration_Node;
          Node : in out WACC.TACKY.Instruction_Node_Vectors.Vector);
      procedure Generate
         (Tree : WACC.AST.Block_Node;
          Node : in out WACC.TACKY.Instruction_Node_Vectors.Vector);
      procedure Generate
         (Tree : WACC.AST.Block_Item_Node;
          Node : in out WACC.TACKY.Instruction_Node_Vectors.Vector);
      procedure Generate
         (Tree : WACC.AST.For_Init_Node;
          Node : in out WACC.TACKY.Instruction_Node_Vectors.Vector);
      procedure Generate
         (Tree : WACC.AST.Statement_Node;
          Node : in out WACC.TACKY.Instruction_Node_Vectors.Vector);
      function Generate
         (Tree : WACC.AST.Exp_Node;
          Node : in out WACC.TACKY.Instruction_Node_Vectors.Vector)
          return Any_Val_Node;
      procedure Generate
         (Tree : WACC.AST.Function_Declaration_Node;
          Node : out WACC.TACKY.Function_Definition_Node);

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
            when WACC.AST.N_Not =>
               return new WACC.TACKY.Unary_Operator_Node'(Typ => TA_Not);
         end case;
      end Convert_Unop;

      function Generate_Unary
         (Tree : WACC.AST.Exp_Node;
          Node : in out WACC.TACKY.Instruction_Node_Vectors.Vector)
          return WACC.TACKY.Any_Val_Node
      is
         Insn : constant WACC.TACKY.Any_Instruction_Node := new WACC.TACKY.Instruction_Node'
            (Typ => WACC.TACKY.TA_Unary,
             Unop_Dst => new Val_Node'
               (Typ  => TA_Var,
                Name => Make_Identifier),
             Unop_Src => Generate (Tree.Exp.all, Node),
             Unary_Operator => Convert_Unop (Tree.Unary_Operator.all));
      begin
         Instruction_Node_Vectors.Append (Node, Insn);
         return Insn.Unop_Dst;
      end Generate_Unary;

      function Generate_Binary
         (Tree : WACC.AST.Exp_Node;
          Node : in out WACC.TACKY.Instruction_Node_Vectors.Vector)
          return WACC.TACKY.Any_Val_Node
      is
         use Instruction_Node_Vectors;
         Result : constant Any_Val_Node := new Val_Node'(Typ => TA_Var, Name => Make_Identifier);
         V1, V2 : Any_Val_Node;
         Insn   : WACC.TACKY.Any_Instruction_Node;

         procedure Generate_Simple_Binary
            (Binop : Binary_Operator_Type)
         is
         begin
            Insn := new Instruction_Node'
               (Typ => TA_Binary,
                Binop_Dst => Result,
                Binop_Src1 => Generate (Tree.Left.all, Node),
                Binop_Src2 => Generate (Tree.Right.all, Node),
                Binary_Operator => new Binary_Operator_Node'(Typ => Binop));
            Append (Node, Insn);
         end Generate_Simple_Binary;
      begin
         case Tree.Binary_Operator.Typ is
            when WACC.AST.N_Add =>
               Generate_Simple_Binary (TA_Add);
            when WACC.AST.N_Subtract =>
               Generate_Simple_Binary (TA_Subtract);
            when WACC.AST.N_Multiply =>
               Generate_Simple_Binary (TA_Multiply);
            when WACC.AST.N_Divide =>
               Generate_Simple_Binary (TA_Divide);
            when WACC.AST.N_Remainder =>
               Generate_Simple_Binary (TA_Remainder);
            when WACC.AST.N_Equal =>
               Generate_Simple_Binary (TA_Equal);
            when WACC.AST.N_Not_Equal =>
               Generate_Simple_Binary (TA_Not_Equal);
            when WACC.AST.N_Less_Than =>
               Generate_Simple_Binary (TA_Less_Than);
            when WACC.AST.N_Less_Or_Equal =>
               Generate_Simple_Binary (TA_Less_Or_Equal);
            when WACC.AST.N_Greater_Than =>
               Generate_Simple_Binary (TA_Greater_Than);
            when WACC.AST.N_Greater_Or_Equal =>
               Generate_Simple_Binary (TA_Greater_Or_Equal);
            when WACC.AST.N_Or =>
               declare
                  True_Label : constant Identifier := Make_Identifier ("or_true.");
                  End_Label  : constant Identifier := Make_Identifier ("or_end.");
               begin
                  V1 := Generate (Tree.Left.all, Node);
                  Append (Node, new Instruction_Node'
                     (Typ           => TA_Jump_If_Not_Zero,
                      JNZ_Condition => V1,
                      JNZ_Target    => True_Label));
                  V2 := Generate (Tree.Right.all, Node);
                  Append (Node, new Instruction_Node'
                     (Typ           => TA_Jump_If_Not_Zero,
                      JNZ_Condition => V2,
                      JNZ_Target    => True_Label));
                  Append (Node, new Instruction_Node'
                     (Typ           => TA_Copy,
                      Copy_Src      => new Val_Node'(Typ => TA_Constant, Int => 0),
                      Copy_Dst      => Result));
                  Append (Node, new Instruction_Node'
                     (Typ           => TA_Jump,
                      J_Target      => End_Label));
                  Append (Node, new Instruction_Node'
                     (Typ           => TA_Label,
                      Label         => True_Label));
                  Append (Node, new Instruction_Node'
                     (Typ           => TA_Copy,
                      Copy_Src      => new Val_Node'(Typ => TA_Constant, Int => 1),
                      Copy_Dst      => Result));
                  Append (Node, new Instruction_Node'
                     (Typ           => TA_Label,
                      Label         => End_Label));
               end;
            when WACC.AST.N_And =>
               declare
                  False_Label : constant Identifier := Make_Identifier ("and_false.");
                  End_Label   : constant Identifier := Make_Identifier ("and_end.");
               begin
                  V1 := Generate (Tree.Left.all, Node);
                  Append (Node, new Instruction_Node'
                     (Typ           => TA_Jump_If_Zero,
                      JZ_Condition  => V1,
                      JZ_Target     => False_Label));
                  V2 := Generate (Tree.Right.all, Node);
                  Append (Node, new Instruction_Node'
                     (Typ           => TA_Jump_If_Zero,
                      JZ_Condition  => V2,
                      JZ_Target     => False_Label));
                  Append (Node, new Instruction_Node'
                     (Typ           => TA_Copy,
                      Copy_Src      => new Val_Node'(Typ => TA_Constant, Int => 1),
                      Copy_Dst      => Result));
                  Append (Node, new Instruction_Node'
                     (Typ           => TA_Jump,
                      J_Target      => End_Label));
                  Append (Node, new Instruction_Node'
                     (Typ           => TA_Label,
                      Label         => False_Label));
                  Append (Node, new Instruction_Node'
                     (Typ           => TA_Copy,
                      Copy_Src      => new Val_Node'(Typ => TA_Constant, Int => 0),
                      Copy_Dst      => Result));
                  Append (Node, new Instruction_Node'
                     (Typ           => TA_Label,
                      Label         => End_Label));
               end;
         end case;
         return Result;
      end Generate_Binary;

      function Generate
         (Tree : WACC.AST.Exp_Node;
          Node : in out WACC.TACKY.Instruction_Node_Vectors.Vector)
          return Any_Val_Node
      is
         use WACC.TACKY.Instruction_Node_Vectors;
      begin
         case Tree.Typ is
            when WACC.AST.N_Constant =>
               return new Val_Node'
                  (Typ => TA_Constant,
                   Int => Tree.Int);
            when WACC.AST.N_Unary =>
               return Generate_Unary (Tree, Node);
            when WACC.AST.N_Binary =>
               return Generate_Binary (Tree, Node);
            when WACC.AST.N_Var =>
               return new Val_Node'
                  (Typ  => TA_Var,
                   Name => Tree.Name);
            when WACC.AST.N_Assignment =>
               declare
                  Dest : constant Any_Val_Node := new Val_Node'
                     (Typ  => TA_Var,
                      Name => Tree.Assign_Left.Name);
               begin
                  Append (Node, new Instruction_Node'
                     (Typ      => TA_Copy,
                      Copy_Src => Generate (Tree.Assign_Right.all, Node),
                      Copy_Dst => Dest));
                  return Dest;
               end;
            when WACC.AST.N_Conditional =>
               declare
                  False_Label : constant Identifier := Make_Identifier ("if_false.");
                  End_Label   : constant Identifier := Make_Identifier ("end_if.");
                  Dest : constant Any_Val_Node := new Val_Node'
                     (Typ  => TA_Var,
                      Name => Make_Identifier ("condition_result."));
                  Condition_Result, Exp_Result : Any_Val_Node;
               begin
                  Condition_Result := Generate (Tree.Condition.all, Node);
                  Append (Node, new Instruction_Node'
                     (Typ          => TA_Jump_If_Zero,
                      JZ_Condition => Condition_Result,
                      JZ_Target    => False_Label));
                  Exp_Result := Generate (Tree.If_True.all, Node);
                  Append (Node, new Instruction_Node'
                     (Typ      => TA_Copy,
                      Copy_Src => Exp_Result,
                      Copy_Dst => Dest));
                  Append (Node, new Instruction_Node'
                     (Typ          => TA_Jump,
                      J_Target     => End_Label));
                  Append (Node, new Instruction_Node'
                     (Typ          => TA_Label,
                      Label        => False_Label));
                  Exp_Result := Generate (Tree.If_False.all, Node);
                  Append (Node, new Instruction_Node'
                     (Typ      => TA_Copy,
                      Copy_Src => Exp_Result,
                      Copy_Dst => Dest));
                  Append (Node, new Instruction_Node'
                     (Typ          => TA_Label,
                      Label        => End_Label));
                  return Dest;
               end;
            when WACC.AST.N_Function_Call =>
               raise Program_Error with "TODO";
         end case;
      end Generate;

      procedure Generate
         (Tree : WACC.AST.For_Init_Node;
          Node : in out WACC.TACKY.Instruction_Node_Vectors.Vector)
      is
         use type WACC.AST.Any_Exp_Node;
      begin
         case Tree.Typ is
            when WACC.AST.N_Init_Expression =>
               if Tree.Exp /= null then
                  declare
                     Result : Any_Val_Node
                        with Unreferenced;
                  begin
                     Result := Generate (Tree.Exp.all, Node);
                  end;
               end if;
            when WACC.AST.N_Init_Declaration =>
               Generate (Tree.Decl.all, Node);
         end case;
      end Generate;

      procedure Generate
         (Tree : WACC.AST.Statement_Node;
          Node : in out WACC.TACKY.Instruction_Node_Vectors.Vector)
      is
         use Instruction_Node_Vectors;
         use type WACC.AST.Any_Exp_Node;
      begin
         case Tree.Typ is
            when WACC.AST.N_Return =>
               Append (Node, new Instruction_Node'
                  (Typ => TA_Return,
                   Val => Generate (Tree.Exp.all, Node)));
            when WACC.AST.N_Expression =>
               declare
                  Result : Any_Val_Node
                     with Unreferenced;
               begin
                  Result := Generate (Tree.Exp.all, Node);
               end;
            when WACC.AST.N_If =>
               declare
                  use type WACC.AST.Any_Statement_Node;
                  False_Label : constant Identifier := Make_Identifier ("if_false.");
                  End_Label   : constant Identifier := Make_Identifier ("end_if.");
                  Condition_Result : Any_Val_Node;
               begin
                  Condition_Result := Generate (Tree.Condition.all, Node);
                  Append (Node, new Instruction_Node'
                     (Typ          => TA_Jump_If_Zero,
                      JZ_Condition => Condition_Result,
                      JZ_Target    => False_Label));
                  Generate (Tree.If_True.all, Node);
                  Append (Node, new Instruction_Node'
                     (Typ          => TA_Jump,
                      J_Target     => End_Label));
                  Append (Node, new Instruction_Node'
                     (Typ          => TA_Label,
                      Label        => False_Label));
                  if Tree.If_False /= null then
                     Generate (Tree.If_False.all, Node);
                  end if;
                  Append (Node, new Instruction_Node'
                     (Typ          => TA_Label,
                      Label        => End_Label));
               end;
            when WACC.AST.N_Compound =>
               Generate (Tree.Block.all, Node);
            when WACC.AST.N_Break =>
               Append (Node, new Instruction_Node'
                  (Typ      => TA_Jump,
                   J_Target => "break_" & Tree.Label));
            when WACC.AST.N_Continue =>
               Append (Node, new Instruction_Node'
                  (Typ      => TA_Jump,
                   J_Target => "continue_" & Tree.Label));
            when WACC.AST.N_DoWhile =>
               Append (Node, new Instruction_Node'
                  (Typ   => TA_Label,
                   Label => Tree.While_Label));
               Generate (Tree.While_Body.all, Node);
               Append (Node, new Instruction_Node'
                  (Typ   => TA_Label,
                   Label => "continue_" & Tree.While_Label));
               Append (Node, new Instruction_Node'
                  (Typ           => TA_Jump_If_Not_Zero,
                   JNZ_Condition => Generate (Tree.While_Condition.all, Node),
                   JNZ_Target    => Tree.While_Label));
               Append (Node, new Instruction_Node'
                  (Typ   => TA_Label,
                   Label => "break_" & Tree.While_Label));
            when WACC.AST.N_While =>
               Append (Node, new Instruction_Node'
                  (Typ   => TA_Label,
                   Label => "continue_" & Tree.While_Label));
               Append (Node, new Instruction_Node'
                  (Typ           => TA_Jump_If_Zero,
                   JZ_Condition  => Generate (Tree.While_Condition.all, Node),
                   JZ_Target     => "break_" & Tree.While_Label));
               Generate (Tree.While_Body.all, Node);
               Append (Node, new Instruction_Node'
                  (Typ        => TA_Jump,
                   J_Target   => "continue_" & Tree.While_Label));
               Append (Node, new Instruction_Node'
                  (Typ   => TA_Label,
                   Label => "break_" & Tree.While_Label));
            when WACC.AST.N_For =>
               Generate (Tree.For_Init.all, Node);
               Append (Node, new Instruction_Node'
                  (Typ   => TA_Label,
                   Label => "start_" & Tree.For_Label));
               if Tree.For_Condition /= null then
                  Append (Node, new Instruction_Node'
                     (Typ           => TA_Jump_If_Zero,
                      JZ_Condition  => Generate (Tree.For_Condition.all, Node),
                      JZ_Target     => "break_" & Tree.For_Label));
               end if;
               Generate (Tree.For_Body.all, Node);
               Append (Node, new Instruction_Node'
                  (Typ   => TA_Label,
                   Label => "continue_" & Tree.For_Label));

               if Tree.For_Post /= null then
                  declare
                     Result : Any_Val_Node
                        with Unreferenced;
                  begin
                     Result := Generate (Tree.For_Post.all, Node);
                  end;
               end if;
               Append (Node, new Instruction_Node'
                  (Typ => TA_Jump,
                   J_Target => "start_" & Tree.For_Label));
               Append (Node, new Instruction_Node'
                  (Typ   => TA_Label,
                   Label => "break_" & Tree.For_Label));
            when WACC.AST.N_Goto =>
               Append (Node, new Instruction_Node'
                  (Typ      => TA_Jump,
                   J_Target => Tree.Label));
            when WACC.AST.N_Label =>
               Append (Node, new Instruction_Node'
                  (Typ   => TA_Label,
                   Label => Tree.Label));
            when WACC.AST.N_Null =>
               null;
         end case;
      end Generate;

      procedure Generate
         (Tree : WACC.AST.Variable_Declaration_Node;
          Node : in out WACC.TACKY.Instruction_Node_Vectors.Vector)
      is
         use type WACC.AST.Any_Exp_Node;
      begin
         if Tree.Init /= null then
            Instruction_Node_Vectors.Append (Node, new Instruction_Node'
               (Typ      => TA_Copy,
                Copy_Src => Generate (Tree.Init.all, Node),
                Copy_Dst => new Val_Node'(Typ => TA_Var, Name => Tree.Name)));
         end if;
      end Generate;

      procedure Generate
         (Tree : WACC.AST.Declaration_Node;
          Node : in out WACC.TACKY.Instruction_Node_Vectors.Vector)
      is
      begin
         case Tree.Typ is
            when WACC.AST.N_FunDecl =>
               raise Program_Error with "TODO";
               --  Generate (Tree.Function_Declaration.all);
            when WACC.AST.N_VarDecl =>
               Generate (Tree.Variable_Declaration.all, Node);
         end case;
      end Generate;

      procedure Generate
         (Tree : WACC.AST.Block_Item_Node;
          Node : in out WACC.TACKY.Instruction_Node_Vectors.Vector)
      is
      begin
         case Tree.Typ is
            when WACC.AST.N_Statement =>
               Generate (Tree.Stmt.all, Node);
            when WACC.AST.N_Declaration =>
               Generate (Tree.Decl.all, Node);
         end case;
      end Generate;

      procedure Generate
         (Tree : WACC.AST.Block_Node;
          Node : in out WACC.TACKY.Instruction_Node_Vectors.Vector)
      is
         use type WACC.AST.Any_Block_Item_Node;
         Item : WACC.AST.Any_Block_Item_Node;
      begin
         Item := Tree.Head;
         while Item /= null loop
            Generate (Item.all, Node);
            Item := Item.Next;
         end loop;
      end Generate;

      procedure Generate
         (Tree : WACC.AST.Function_Declaration_Node;
          Node : out WACC.TACKY.Function_Definition_Node)
      is
         use type WACC.AST.Any_Block_Node;
      begin
         Node.Name := Tree.Name;
         if Tree.FBody /= null then
            Generate (Tree.FBody.all, Node.FBody);
            Instruction_Node_Vectors.Append (Node.FBody, new Instruction_Node'
               (Typ => TA_Return,
                Val => new Val_Node'
                  (Typ => TA_Constant,
                   Int => 0)));
         end if;
      end Generate;
   begin
      for Decl of Tree.Function_Declarations loop
         Generate (Decl.all, Node.Function_Definition);
         --  TODO maybe not put every declaration in one TACKY.Function_Definition?
      end loop;
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
         (Node : WACC.Strings.Identifier)
      is
      begin
         Write (To_String (Node));
      end Print;

      procedure Print
         (Node : WACC.TACKY.Binary_Operator_Node)
      is
      begin
         Write ("Binary_Operator");
         Indent;
         Write (Node.Typ'Image);
         Dedent;
      end Print;

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
               Print (Node.Unop_Dst.all);
               Write ("Src = ");
               Print (Node.Unop_Src.all);
            when TA_Binary =>
               Write ("Binary_Operator =");
               Print (Node.Binary_Operator.all);
               Write ("Dst = ");
               Print (Node.Binop_Dst.all);
               Write ("Src1 = ");
               Print (Node.Binop_Src1.all);
               Write ("Src2 = ");
               Print (Node.Binop_Src1.all);
            when TA_Copy =>
               Write ("Src = ");
               Print (Node.Copy_Src.all);
               Write ("Dst = ");
               Print (Node.Copy_Dst.all);
            when TA_Jump =>
               Write ("Target = ");
               Print (Node.J_Target);
            when TA_Jump_If_Zero =>
               Write ("JZ_Condition = ");
               Print (Node.JZ_Condition.all);
               Write ("JZ_Target = ");
               Print (Node.JZ_Target);
            when TA_Jump_If_Not_Zero =>
               Write ("JNZ_Condition = ");
               Print (Node.JNZ_Condition.all);
               Write ("JNZ_Target = ");
               Print (Node.JNZ_Target);
            when TA_Label =>
               Write ("Label = ");
               Print (Node.Label);
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
