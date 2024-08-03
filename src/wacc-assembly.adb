pragma Style_Checks ("M120");
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Text_IO;
with WACC.IO;

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

      procedure Generate_Binary_Operation
         (Tree : WACC.TACKY.Instruction_Node;
          Node : in out WACC.Assembly.Instruction_Node_Vectors.Vector)
      is
         use WACC.Assembly.Instruction_Node_Vectors;
         Dst : Any_Operand_Node;
      begin
         case Tree.Binary_Operator.Typ is
            when WACC.TACKY.TA_Add =>
               Dst := Convert_Operand (Tree.Binop_Dst.all);
               Append (Node, new Instruction_Node'
                  (Typ => A_Mov,
                   Src => Convert_Operand (Tree.Binop_Src1.all),
                   Dst => Dst));
               Append (Node, new Instruction_Node'
                  (Typ => A_Binary,
                   Binary_Operator => new Binary_Operator_Node'(Typ => A_Add),
                   Binary_Src => Convert_Operand (Tree.Binop_Src2.all),
                   Binary_Dst => Dst));
            when WACC.TACKY.TA_Subtract =>
               Dst := Convert_Operand (Tree.Binop_Dst.all);
               Append (Node, new Instruction_Node'
                  (Typ => A_Mov,
                   Src => Convert_Operand (Tree.Binop_Src1.all),
                   Dst => Dst));
               Append (Node, new Instruction_Node'
                  (Typ => A_Binary,
                   Binary_Operator => new Binary_Operator_Node'(Typ => A_Sub),
                   Binary_Src => Convert_Operand (Tree.Binop_Src2.all),
                   Binary_Dst => Dst));
            when WACC.TACKY.TA_Multiply =>
               Dst := Convert_Operand (Tree.Binop_Dst.all);
               Append (Node, new Instruction_Node'
                  (Typ => A_Mov,
                   Src => Convert_Operand (Tree.Binop_Src1.all),
                   Dst => Dst));
               Append (Node, new Instruction_Node'
                  (Typ => A_Binary,
                   Binary_Operator => new Binary_Operator_Node'(Typ => A_Mult),
                   Binary_Src => Convert_Operand (Tree.Binop_Src2.all),
                   Binary_Dst => Dst));
            when WACC.TACKY.TA_Divide =>
               Dst := new Operand_Node'
                  (Typ => A_Reg,
                   Reg => new Reg_Node'(Typ => A_AX));
               Append (Node, new Instruction_Node'
                  (Typ => A_Mov,
                   Src => Convert_Operand (Tree.Binop_Src1.all),
                   Dst => Dst));
               Append (Node, new Instruction_Node'(Typ => A_Cdq));
               Append (Node, new Instruction_Node'
                  (Typ => A_Idiv,
                   Idiv_Src => Convert_Operand (Tree.Binop_Src2.all)));
               Append (Node, new Instruction_Node'
                  (Typ => A_Mov,
                   Src => Dst,
                   Dst => Convert_Operand (Tree.Binop_Dst.all)));
            when WACC.TACKY.TA_Remainder =>
               Append (Node, new Instruction_Node'
                  (Typ => A_Mov,
                   Src => Convert_Operand (Tree.Binop_Src1.all),
                   Dst => new Operand_Node'(Typ => A_Reg, Reg => new Reg_Node'(Typ => A_AX))));
               Append (Node, new Instruction_Node'(Typ => A_Cdq));
               Append (Node, new Instruction_Node'
                  (Typ => A_Idiv,
                   Idiv_Src => Convert_Operand (Tree.Binop_Src2.all)));
               Append (Node, new Instruction_Node'
                  (Typ => A_Mov,
                   Src => new Operand_Node'(Typ => A_Reg, Reg => new Reg_Node'(Typ => A_DX)),
                   Dst => Convert_Operand (Tree.Binop_Dst.all)));
         end case;
      end Generate_Binary_Operation;

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
                  Dst : constant Any_Operand_Node := Convert_Operand (Tree.Unop_Dst.all);
               begin
                  Append (Node, new WACC.Assembly.Instruction_Node'
                     (Typ => WACC.Assembly.A_Mov,
                      Src => Convert_Operand (Tree.Unop_Src.all),
                      Dst => Dst));
                  Append (Node, new WACC.Assembly.Instruction_Node'
                     (Typ => WACC.Assembly.A_Unary,
                      Unary_Operator => Convert_Unary_Operator (Tree.Unary_Operator.all),
                      Unary_Operand => Dst));
               end;
            when WACC.TACKY.TA_Binary =>
               Generate_Binary_Operation (Tree, Node);
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
               Name : constant String := To_String (Node.Name);
            begin
               if not Contains (Pseudo_Map, Name) then
                  Insert (Pseudo_Map, Name, Next_Stack_Offset);
                  Next_Stack_Offset := Next_Stack_Offset - 4;
               end if;
               Node := new Operand_Node'
                  (Typ => A_Stack,
                   Stack_Int => Element (Pseudo_Map, Name));
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
               Replace_Pseudo (Node.Unary_Operand);
            when A_Binary =>
               Replace_Pseudo (Node.Binary_Src);
               Replace_Pseudo (Node.Binary_Dst);
            when A_Idiv =>
               Replace_Pseudo (Node.Idiv_Src);
            when A_Cdq | A_Allocate_Stack | A_Ret =>
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

      procedure Stack_Fixup
         (Node : in out WACC.Assembly.Function_Definition_Node)
      is
         use Instruction_Node_Vectors;
         Scratch_Reg : constant Any_Operand_Node := new Operand_Node'
            (Typ => A_Reg, Reg => new Reg_Node'(Typ => A_R10));

         type Edit is record
            After : Natural;
            Scratch_Mov : Any_Instruction_Node;
         end record;

         package Edit_Vectors is new Ada.Containers.Vectors
            (Positive, Edit);
         use Edit_Vectors;
         Edits : Edit_Vectors.Vector;
      begin
         for Cursor in Iterate (Node.Instructions) loop
            declare
               Insn : constant Any_Instruction_Node := Element (Cursor);
               I : constant Natural := To_Index (Cursor);
               Scratch_Mov : Any_Instruction_Node;
            begin
               if Insn.Typ = A_Mov and then
                  Insn.Src.Typ = A_Stack and then
                  Insn.Dst.Typ = A_Stack
               then
                  Scratch_Mov := new Instruction_Node'
                     (Typ => A_Mov,
                      Src => Scratch_Reg,
                      Dst => Insn.Dst);
                  Insn.Dst := Scratch_Reg;
                  Append (Edits, (After => I, Scratch_Mov => Scratch_Mov));
               end if;
            end;
         end loop;

         for E of reverse Edits loop
            if E.After = Last_Index (Node.Instructions) then
               Append (Node.Instructions, E.Scratch_Mov);
            else
               Insert (Node.Instructions, Before => E.After + 1, New_Item => E.Scratch_Mov);
            end if;
         end loop;

         Prepend (Node.Instructions, new Instruction_Node'
            (Typ => A_Allocate_Stack,
             Stack_Size => abs Integer (Next_Stack_Offset - 4)));
      end Stack_Fixup;

      procedure Binop_Fixup
         (Node : in out WACC.Assembly.Function_Definition_Node)
      is
         use Instruction_Node_Vectors;
         Rewrite : Instruction_Node_Vectors.Vector := Instruction_Node_Vectors.Empty_Vector;
         Tmp : Any_Operand_Node;
      begin
         for Insn of Node.Instructions loop
            if Insn.Typ = A_Idiv and then
               Insn.Idiv_Src.Typ = A_Imm
            then
               --  idiv does not take immediate operands, rewrite to use an intermediate register
               Tmp := new Operand_Node'(Typ => A_Reg, Reg => new Reg_Node'(Typ => A_R10));
               Append (Rewrite, new Instruction_Node'(Typ => A_Mov, Src => Insn.Idiv_Src, Dst => Tmp));
               Append (Rewrite, new Instruction_Node'(Typ => A_Idiv, Idiv_Src => Tmp));
            elsif Insn.Typ = A_Binary and then
                  Insn.Binary_Operator.Typ in A_Add .. A_Sub and then
                  Insn.Binary_Src.Typ = A_Stack and then
                  Insn.Binary_Dst.Typ = A_Stack
            then
               --  add and sub cannot use memory for both operands, rewrite src to use an intermediate register
               Tmp := new Operand_Node'(Typ => A_Reg, Reg => new Reg_Node'(Typ => A_R10));
               Append (Rewrite, new Instruction_Node'(Typ => A_Mov, Src => Insn.Binary_Src, Dst => Tmp));
               Append (Rewrite, new Instruction_Node'
                  (Typ => A_Binary,
                   Binary_Operator => Insn.Binary_Operator,
                   Binary_Src => Tmp,
                   Binary_Dst => Insn.Binary_Dst));
            elsif Insn.Typ = A_Binary and then
                  Insn.Binary_Operator.Typ = A_Mult and then
                  Insn.Binary_Dst.Typ = A_Stack
            then
               --  imul cannot use a memory address as dst, rewrite it to use an intermediate register
               Tmp := new Operand_Node'(Typ => A_Reg, Reg => new Reg_Node'(Typ => A_R11));
               Append (Rewrite, new Instruction_Node'(Typ => A_Mov, Src => Insn.Binary_Dst, Dst => Tmp));
               Append (Rewrite, new Instruction_Node'
                  (Typ => A_Binary,
                   Binary_Operator => Insn.Binary_Operator,
                   Binary_Src => Insn.Binary_Src,
                   Binary_Dst => Tmp));
               Append (Rewrite, new Instruction_Node'(Typ => A_Mov, Src => Tmp, Dst => Insn.Binary_Dst));
            else
               Append (Rewrite, Insn);
            end if;
         end loop;

         --  Finally replace Node.Instructions with Rewrite
         Move (Target => Node.Instructions, Source => Rewrite);
      end Binop_Fixup;
   begin
      --  Pass 1: TACKY to Assembly
      Generate (Tree.Function_Definition, Asm.Function_Definition);

      --  Pass 2: Replacing Pseudoregisters with Stack locations
      Replace_Pseudo (Asm.Function_Definition);

      --  Pass 3: Stack Fixup
      Stack_Fixup (Asm.Function_Definition);

      --  Pass 4: Binary operation register fixup
      Binop_Fixup (Asm.Function_Definition);
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
         (Node : WACC.Assembly.Binary_Operator_Node)
      is
      begin
         Write ("Binary_Operator");
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
               Print (Node.Unary_Operand.all);
            when A_Binary =>
               Write ("Binary_Operator = ");
               Print (Node.Binary_Operator.all);
               Write ("Src = ");
               Print (Node.Binary_Src.all);
               Write ("Dst = ");
               Print (Node.Binary_Dst.all);
            when A_Idiv =>
               Write ("Src = ");
               Print (Node.Idiv_Src.all);
            when A_Cdq =>
               null;
            when A_Allocate_Stack =>
               Write ("Size = ");
               Write (Node.Stack_Size'Image);
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

   procedure Emit
      (Node : WACC.Assembly.Program_Node;
       Filename : String)
   is
      File : WACC.IO.Writer;

      procedure Indent is
      begin
         WACC.IO.Put (File, "    ");
      end Indent;

      procedure Write
         (Str : String)
      is
      begin
         WACC.IO.Put (File, Str);
      end Write;

      procedure Write
         (N : Long_Integer)
      is
      begin
         WACC.IO.Put (File, N);
      end Write;

      procedure New_Line is
      begin
         WACC.IO.Put (File, ASCII.LF);
      end New_Line;

      procedure Print
         (Node : WACC.Assembly.Reg_Node)
      is
      begin
         case Node.Typ is
            when A_AX =>
               Write ("eax");
            when A_DX =>
               Write ("edx");
            when A_R10 =>
               Write ("r10d");
            when A_R11 =>
               Write ("r11d");
         end case;
      end Print;

      procedure Print
         (Node : WACC.Assembly.Operand_Node)
      is
      begin
         case Node.Typ is
            when A_Imm =>
               Write ("$");
               Write (Node.Imm_Int);
            when A_Reg =>
               Write ("%");
               Print (Node.Reg.all);
            when A_Stack =>
               Write (Long_Integer (Node.Stack_Int));
               Write ("(%rbp)");
            when A_Pseudo =>
               raise Assembly_Error with "Cannot emit pseudo instruction to file";
         end case;
      end Print;

      procedure Print
         (Node : WACC.Assembly.Unary_Operator_Node)
      is
      begin
         case Node.Typ is
            when A_Neg =>
               Write ("negl");
            when A_Not =>
               Write ("notl");
         end case;
      end Print;

      procedure Print
         (Node : WACC.Assembly.Binary_Operator_Node)
      is
      begin
         case Node.Typ is
            when A_Add =>
               Write ("addl");
            when A_Sub =>
               Write ("subl");
            when A_Mult =>
               Write ("imull");
         end case;
      end Print;

      procedure Print
         (Node : WACC.Assembly.Instruction_Node)
      is
      begin
         Indent;
         case Node.Typ is
            when A_Mov =>
               Write ("movl ");
               Print (Node.Src.all);
               Write (", ");
               Print (Node.Dst.all);
            when A_Unary =>
               Print (Node.Unary_Operator.all);
               Write (" ");
               Print (Node.Unary_Operand.all);
            when A_Binary =>
               Print (Node.Binary_Operator.all);
               Write (" ");
               Print (Node.Binary_Src.all);
               Write (", ");
               Print (Node.Binary_Dst.all);
            when A_Cdq =>
               Write ("cdq");
            when A_Idiv =>
               Write ("idivl ");
               Print (Node.Idiv_Src.all);
            when A_Allocate_Stack =>
               Write ("subq $");
               Write (Long_Integer (Node.Stack_Size));
               Write (", %rsp");
            when A_Ret =>
               Write ("movq %rbp, %rsp");
               New_Line;
               Indent;
               Write ("popq %rbp");
               New_Line;
               Indent;
               Write ("ret");
         end case;
         New_Line;
      end Print;

      procedure Print
         (Node : WACC.Assembly.Function_Definition_Node)
      is
         Label : constant String := To_String (Node.Name);
      begin
         Indent;
         Write (".globl ");
         Write (Label);
         New_Line;

         Write (Label);
         Write (":");
         New_Line;

         Indent;
         Write ("pushq %rbp");
         New_Line;

         Indent;
         Write ("movq %rsp, %rbp");
         New_Line;

         for Insn of Node.Instructions loop
            Print (Insn.all);
         end loop;
      end Print;
   begin
      WACC.IO.Open (File, Filename);

      Print (Node.Function_Definition);

      Indent;
      Write (".section .node.GNU-stack,"""",@progbits");
      New_Line;

      WACC.IO.Close (File);
   end Emit;

end WACC.Assembly;
