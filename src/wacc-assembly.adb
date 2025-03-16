pragma Style_Checks ("M120");
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Text_IO;
with WACC.IO;

package body WACC.Assembly is

   --  The first six function arguments are passed in registers, per the C
   --  calling convention
   type Argument_Register_Index is range 1 .. 6;
   Argument_Register : constant array (Argument_Register_Index) of Reg_Node_Type :=
      (A_DI, A_SI, A_DX, A_CX, A_R8, A_R9);

   type Register_Width is (RW_8, RW_32, RW_64);

   function Convert_Operand
      (Tree : WACC.TACKY.Val_Node)
      return WACC.Assembly.Any_Operand_Node;
   function Convert_Condition
      (Op : WACC.TACKY.Binary_Operator_Node)
      return WACC.Assembly.Condition_Code;
   procedure Generate_Function_Call
      (Tree : WACC.TACKY.Instruction_Node;
       Node : in out WACC.Assembly.Instruction_Node_Vectors.Vector);
   procedure Generate_Unary_Operation
      (Tree : WACC.TACKY.Instruction_Node;
       Node : in out WACC.Assembly.Instruction_Node_Vectors.Vector);
   procedure Generate_Binary_Operation
      (Tree : WACC.TACKY.Instruction_Node;
       Node : in out WACC.Assembly.Instruction_Node_Vectors.Vector);
   procedure Generate
      (Tree : WACC.TACKY.Instruction_Node;
       Node : in out WACC.Assembly.Instruction_Node_Vectors.Vector);
   procedure Generate
      (Tree : WACC.TACKY.Function_Definition_Node;
       Asm  : out WACC.Assembly.Function_Definition_Node);
   procedure Replace_Pseudo
      (Node : in out WACC.Assembly.Any_Operand_Node);
   procedure Replace_Pseudo
      (Node : in out WACC.Assembly.Instruction_Node);
   procedure Replace_Pseudo
      (Node : in out WACC.Assembly.Function_Definition_Node);
   procedure Stack_Fixup
      (Node : in out WACC.Assembly.Function_Definition_Node);
   procedure Binop_Fixup
      (Node : in out WACC.Assembly.Function_Definition_Node);

   package Stack_Maps is new Ada.Containers.Indefinite_Ordered_Maps
      (Key_Type     => String,
       Element_Type => Stack_Offset);
   Pseudo_Map : Stack_Maps.Map := Stack_Maps.Empty_Map;
   Next_Stack_Offset : Stack_Offset;

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

   procedure Generate_Unary_Operation
      (Tree : WACC.TACKY.Instruction_Node;
       Node : in out WACC.Assembly.Instruction_Node_Vectors.Vector)
   is
      use WACC.Assembly.Instruction_Node_Vectors;
      Dst : constant Any_Operand_Node := Convert_Operand (Tree.Unop_Dst.all);
   begin
      case Tree.Unary_Operator.Typ is
         when WACC.TACKY.TA_Complement =>
            Append (Node, new Instruction_Node'
               (Typ => A_Mov,
                Src => Convert_Operand (Tree.Unop_Src.all),
                Dst => Dst));
            Append (Node, new Instruction_Node'
               (Typ => A_Unary,
                Unary_Operator => new Unary_Operator_Node'(Typ => A_Not),
                Unary_Operand => Dst));
         when WACC.TACKY.TA_Negate =>
            Append (Node, new Instruction_Node'
               (Typ => A_Mov,
                Src => Convert_Operand (Tree.Unop_Src.all),
                Dst => Dst));
            Append (Node, new Instruction_Node'
               (Typ => A_Unary,
                Unary_Operator => new Unary_Operator_Node'(Typ => A_Neg),
                Unary_Operand => Dst));
         when WACC.TACKY.TA_Not =>
            Append (Node, new Instruction_Node'
               (Typ => A_Cmp,
                A   => Convert_Operand (Tree.Unop_Src.all),
                B   => new Operand_Node'(Typ => A_Imm, Imm_Int => 0)));
            Append (Node, new Instruction_Node'
               (Typ => A_Mov,
                Src => new Operand_Node'(Typ => A_Imm, Imm_Int => 0),
                Dst => Dst));
            Append (Node, new Instruction_Node'
               (Typ => A_SetCC,
                SetCC_Condition => E,
                SetCC_Operand => Dst));
      end case;
   end Generate_Unary_Operation;

   function Convert_Condition
      (Op : WACC.TACKY.Binary_Operator_Node)
      return WACC.Assembly.Condition_Code
   is
   begin
      case Op.Typ is
         when WACC.TACKY.TA_Equal =>
            return E;
         when WACC.TACKY.TA_Not_Equal =>
            return NE;
         when WACC.TACKY.TA_Less_Than =>
            return L;
         when WACC.TACKY.TA_Less_Or_Equal =>
            return LE;
         when WACC.TACKY.TA_Greater_Than =>
            return G;
         when WACC.TACKY.TA_Greater_Or_Equal =>
            return GE;
         when others =>
            raise Assembly_Error with "Binary operator is not a condition: " & Op.Typ'Image;
      end case;
   end Convert_Condition;

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
         when WACC.TACKY.TA_Equal .. WACC.TACKY.TA_Greater_Or_Equal =>
            Dst := Convert_Operand (Tree.Binop_Dst.all);
            Append (Node, new Instruction_Node'
               (Typ => A_Cmp,
                A => Convert_Operand (Tree.Binop_Src1.all),
                B => Convert_Operand (Tree.Binop_Src2.all)));
            Append (Node, new Instruction_Node'
               (Typ => A_Mov,
                Src => new Operand_Node'(Typ => A_Imm, Imm_Int => 0),
                Dst => Dst));
            Append (Node, new Instruction_Node'
               (Typ => A_SetCC,
                SetCC_Condition => Convert_Condition (Tree.Binary_Operator.all),
                SetCC_Operand => Dst));
      end case;
   end Generate_Binary_Operation;

   procedure Generate_Function_Call
      (Tree : WACC.TACKY.Instruction_Node;
       Node : in out WACC.Assembly.Instruction_Node_Vectors.Vector)
   is
      use WACC.TACKY.Val_Node_Vectors;
      use type Ada.Containers.Count_Type;
      Register_Args, Stack_Args : WACC.TACKY.Val_Node_Vectors.Vector;
      Arg : WACC.TACKY.Any_Val_Node;
      Stack_Padding : Natural := 0;
      I : Positive := 1;
   begin
      for Arg of Tree.Args loop
         if I <= 6 then
            Append (Register_Args, Arg);
         else
            Prepend (Stack_Args, Arg);
         end if;
         I := I + 1;
      end loop;

      --  Align stack
      if (Length (Stack_Args) mod 2) /= 0 then
         Stack_Padding := 8;
         Instruction_Node_Vectors.Append (Node, new Instruction_Node'
            (Typ        => A_Allocate_Stack,
             Stack_Size => Stack_Padding));
      end if;

      --  Save the first six arguments to registers
      for AR of Argument_Register loop
         exit when Is_Empty (Register_Args);
         Arg := First_Element (Register_Args);
         Delete_First (Register_Args);
         Instruction_Node_Vectors.Append (Node, new Instruction_Node'
            (Typ => A_Mov,
             Src => Convert_Operand (Arg.all),
             Dst => new Operand_Node'
               (Typ => A_Reg,
                Reg => new Reg_Node'(Typ => AR))));
      end loop;

      --  Push remaining arguments to stack in reverse order
      while not Is_Empty (Stack_Args) loop
         Arg := First_Element (Stack_Args);
         Delete_First (Stack_Args);
         declare
            Operand : constant Any_Operand_Node := Convert_Operand (Arg.all);
         begin
            if Operand.Typ = A_Imm then
               Instruction_Node_Vectors.Append (Node, new Instruction_Node'
                  (Typ => A_Push,
                   Push_Operand => Convert_Operand (Arg.all)));
            else
               Instruction_Node_Vectors.Append (Node, new Instruction_Node'
                  (Typ => A_Mov,
                   Src => Operand,
                   Dst => new Operand_Node'(Typ => A_Reg, Reg => new Reg_Node'(Typ => A_AX))));
               Instruction_Node_Vectors.Append (Node, new Instruction_Node'
                  (Typ => A_Push,
                   Push_Operand => new Operand_Node'(Typ => A_Reg, Reg => new Reg_Node'(Typ => A_AX))));
            end if;
         end;
      end loop;

      --  Call
      Instruction_Node_Vectors.Append (Node, new Instruction_Node'
         (Typ => A_Call,
          Call_Name => Tree.Fun_Name));

      --  Restore the stack
      declare
         Bytes_To_Remove : constant Natural :=
            8 * Natural (Length (Stack_Args)) + Stack_Padding;
      begin
         if Bytes_To_Remove > 0 then
            Instruction_Node_Vectors.Append (Node, new Instruction_Node'
               (Typ => A_Deallocate_Stack,
                Stack_Size => Bytes_To_Remove));
         end if;
      end;

      --  Pop return value
      Instruction_Node_Vectors.Append (Node, new Instruction_Node'
         (Typ => A_Mov,
          Src => new Operand_Node'
            (Typ => A_Reg,
             Reg => new Reg_Node'(Typ => A_AX)),
          Dst => Convert_Operand (Tree.Dst.all)));
   end Generate_Function_Call;

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
            Generate_Unary_Operation (Tree, Node);
         when WACC.TACKY.TA_Binary =>
            Generate_Binary_Operation (Tree, Node);
         when WACC.TACKY.TA_Copy =>
            Append (Node, new Instruction_Node'
               (Typ => A_Mov,
                Src => Convert_Operand (Tree.Copy_Src.all),
                Dst => Convert_Operand (Tree.Copy_Dst.all)));
         when WACC.TACKY.TA_Jump =>
            Append (Node, new Instruction_Node'
               (Typ => A_Jmp,
                Jmp_Label => Tree.J_Target));
         when WACC.TACKY.TA_Jump_If_Zero =>
            Append (Node, new Instruction_Node'
               (Typ => A_Cmp,
                A => Convert_Operand (Tree.JZ_Condition.all),
                B => new Operand_Node'(Typ => A_Imm, Imm_Int => 0)));
            Append (Node, new Instruction_Node'
               (Typ => A_JmpCC,
                JmpCC_Condition => E,
                JmpCC_Label => Tree.JZ_Target));
         when WACC.TACKY.TA_Jump_If_Not_Zero =>
            Append (Node, new Instruction_Node'
               (Typ => A_Cmp,
                A => new Operand_Node'(Typ => A_Imm, Imm_Int => 0),
                B => Convert_Operand (Tree.JNZ_Condition.all)));
            Append (Node, new Instruction_Node'
               (Typ => A_JmpCC,
                JmpCC_Condition => NE,
                JmpCC_Label => Tree.JNZ_Target));
         when WACC.TACKY.TA_Label =>
            Append (Node, new Instruction_Node'
               (Typ => A_Label,
                Label => Tree.Label));
         when WACC.TACKY.TA_FunCall =>
            Generate_Function_Call (Tree, Node);
      end case;
   end Generate;

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
               Next_Stack_Offset := Next_Stack_Offset - 4;
               Insert (Pseudo_Map, Name, Next_Stack_Offset);
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
         when A_Cmp =>
            Replace_Pseudo (Node.A);
            Replace_Pseudo (Node.B);
         when A_SetCC =>
            Replace_Pseudo (Node.SetCC_Operand);
         when A_Push =>
            Replace_Pseudo (Node.Push_Operand);
         when A_Cdq | A_Allocate_Stack | A_Deallocate_Stack | A_Call |
              A_Ret | A_Jmp | A_JmpCC | A_Label =>
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
          Stack_Size => abs Integer (Next_Stack_Offset)));
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
         elsif Insn.Typ = A_Cmp and then
               Insn.A.Typ = A_Stack and then
               Insn.B.Typ = A_Stack
         then
            --  cmpl cannot use memory for both operands
            Tmp := new Operand_Node'(Typ => A_Reg, Reg => new Reg_Node'(Typ => A_R10));
            Append (Rewrite, new Instruction_Node'(Typ => A_Mov, Src => Insn.B, Dst => Tmp));
            Append (Rewrite, new Instruction_Node'
               (Typ => A_Cmp,
                A => Insn.A,
                B => Tmp));
            Append (Rewrite, new Instruction_Node'(Typ => A_Mov, Src => Tmp, Dst => Insn.B));
         elsif Insn.Typ = A_Cmp and then
               Insn.A.Typ = A_Imm
         then
            --  cmpl b, a operand 'a' cannot be immediate, move it to a register
            Tmp := new Operand_Node'(Typ => A_Reg, Reg => new Reg_Node'(Typ => A_R11));
            Append (Rewrite, new Instruction_Node'(Typ => A_Mov, Src => Insn.A, Dst => Tmp));
            Append (Rewrite, new Instruction_Node'
               (Typ => A_Cmp,
                A => Tmp,
                B => Insn.B));
         else
            Append (Rewrite, Insn);
         end if;
      end loop;

      --  Finally replace Node.Instructions with Rewrite
      Move (Target => Node.Instructions, Source => Rewrite);
   end Binop_Fixup;

   procedure Generate
      (Tree : WACC.TACKY.Function_Definition_Node;
       Asm  : out WACC.Assembly.Function_Definition_Node)
   is
   begin
      Next_Stack_Offset := 0;
      Stack_Maps.Clear (Pseudo_Map);

      --  Pass 1: TACKY to Assembly
      Asm.Name := Tree.Name;

      --  Copy register passed arguments to the stack
      declare
         use Instruction_Node_Vectors;
         use WACC.TACKY.Identifier_Vectors;
         Params : WACC.TACKY.Identifier_Vectors.Vector := Copy (Tree.Params);
         Offset : Stack_Offset := 16;
      begin
         for AR of Argument_Register loop
            exit when Is_Empty (Params);
            Append (Asm.Instructions, new Instruction_Node'
               (Typ => A_Mov,
                Src => new Operand_Node'(Typ => A_Reg, Reg => new Reg_Node'(Typ => AR)),
                Dst => new Operand_Node'
                  (Typ  => A_Pseudo,
                   Name => First_Element (Params))));
            Delete_First (Params);
         end loop;

         while not Is_Empty (Params) loop
            Append (Asm.Instructions, new Instruction_Node'
               (Typ => A_Mov,
                Src => new Operand_Node'
                  (Typ       => A_Stack,
                   Stack_Int => Offset),
                Dst => new Operand_Node'
                  (Typ  => A_Pseudo,
                   Name => First_Element (Params))));
            Delete_First (Params);
            Offset := Offset + 8;
         end loop;
      end;

      for TACKY_Insn of Tree.FBody loop
         Generate (TACKY_Insn.all, Asm.Instructions);
      end loop;

      --  Pass 2: Replacing Pseudoregisters with Stack locations
      Replace_Pseudo (Asm);

      --  Pass 3: Stack Fixup
      Stack_Fixup (Asm);

      --  Pass 4: Binary operation register fixup
      Binop_Fixup (Asm);
   end Generate;

   procedure Generate
      (Tree : WACC.TACKY.Program_Node;
       Asm  : out Program_Node)
   is
      FN : Any_Function_Definition_Node;
   begin
      for Def of Tree.Function_Definitions loop
         FN := new Function_Definition_Node;
         Generate (Def.all, FN.all);
         Function_Definition_Node_Vectors.Append (Asm.Function_Definitions, FN);
      end loop;
   end Generate;

   procedure Print
      (Node : WACC.Assembly.Program_Node)
   is
      Indent_Level : Natural := 0;

      procedure Write
         (Str : String);
      procedure Indent;
      procedure Dedent;
      procedure Print
         (Node : Identifier);
      procedure Print
         (Node : WACC.Assembly.Reg_Node);
      procedure Print
         (Node : WACC.Assembly.Operand_Node);
      procedure Print
         (Node : WACC.Assembly.Unary_Operator_Node);
      procedure Print
         (Node : WACC.Assembly.Binary_Operator_Node);
      procedure Print
         (Node : WACC.Assembly.Instruction_Node);
      procedure Print
         (Node : WACC.Assembly.Function_Definition_Node);

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
         (Node : Identifier)
      is
      begin
         Indent;
         Write (To_String (Node));
         Dedent;
      end Print;

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
            when A_Cmp =>
               Write ("A = ");
               Print (Node.A.all);
               Write ("B = ");
               Print (Node.B.all);
            when A_Idiv =>
               Write ("Src = ");
               Print (Node.Idiv_Src.all);
            when A_Cdq =>
               null;
            when A_Jmp =>
               Write ("Label = ");
               Print (Node.Jmp_Label);
            when A_JmpCC =>
               Write ("Condition = ");
               Write (Node.JmpCC_Condition'Image);
               Write ("Label = ");
               Print (Node.JmpCC_Label);
            when A_SetCC =>
               Write ("Condition = ");
               Write (Node.SetCC_Condition'Image);
               Write ("Operand = ");
               Print (Node.SetCC_Operand.all);
            when A_Label =>
               Print (Node.Label);
            when A_Allocate_Stack | A_Deallocate_Stack =>
               Write ("Size = ");
               Write (Node.Stack_Size'Image);
            when A_Push =>
               Write ("Operand = ");
               Print (Node.Push_Operand.all);
            when A_Call =>
               Write ("Name = ");
               Print (Node.Call_Name);
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
      for Def of Node.Function_Definitions loop
         Print (Def.all);
      end loop;
      Dedent;
      Dedent;
   end Print;

   procedure Emit
      (Node : WACC.Assembly.Program_Node;
       Filename : String)
   is
      procedure Indent;
      procedure Write
         (Str : String);
      procedure Write
         (N : Long_Integer);
      procedure New_Line;
      procedure Print
         (Node  : WACC.Assembly.Reg_Node;
          Width : Register_Width);
      procedure Print
         (Node  : WACC.Assembly.Operand_Node;
          Width : Register_Width := RW_32);
      procedure Print
         (Node : WACC.Assembly.Unary_Operator_Node);
      procedure Print
         (Node : WACC.Assembly.Binary_Operator_Node);
      procedure Print
         (Cond : Condition_Code);
      procedure Print
         (Name : Identifier);
      procedure Print
         (Node : WACC.Assembly.Instruction_Node);
      procedure Print
         (Node : WACC.Assembly.Function_Definition_Node);

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
         (Node  : WACC.Assembly.Reg_Node;
          Width : Register_Width)
      is
      begin
         case Node.Typ is
            when A_AX =>
               case Width is
                  when RW_8  => Write ("al");
                  when RW_32 => Write ("eax");
                  when RW_64 => Write ("rax");
               end case;
            when A_CX =>
               case Width is
                  when RW_8  => Write ("cl");
                  when RW_32 => Write ("ecx");
                  when RW_64 => Write ("rcx");
               end case;
            when A_DX =>
               case Width is
                  when RW_8  => Write ("dl");
                  when RW_32 => Write ("edx");
                  when RW_64 => Write ("rdx");
               end case;
            when A_DI =>
               case Width is
                  when RW_8  => Write ("dil");
                  when RW_32 => Write ("edi");
                  when RW_64 => Write ("rdi");
               end case;
            when A_SI =>
               case Width is
                  when RW_8  => Write ("sil");
                  when RW_32 => Write ("esi");
                  when RW_64 => Write ("rsi");
               end case;
            when A_R8 =>
               case Width is
                  when RW_8  => Write ("r8b");
                  when RW_32 => Write ("r8d");
                  when RW_64 => Write ("r8");
               end case;
            when A_R9 =>
               case Width is
                  when RW_8  => Write ("r9b");
                  when RW_32 => Write ("r9d");
                  when RW_64 => Write ("r9");
               end case;
            when A_R10 =>
               case Width is
                  when RW_8  => Write ("r10b");
                  when RW_32 => Write ("r10d");
                  when RW_64 => Write ("r10");
               end case;
            when A_R11 =>
               case Width is
                  when RW_8  => Write ("r11b");
                  when RW_32 => Write ("r11d");
                  when RW_64 => Write ("r11");
               end case;
         end case;
      end Print;

      procedure Print
         (Node  : WACC.Assembly.Operand_Node;
          Width : Register_Width := RW_32)
      is
      begin
         case Node.Typ is
            when A_Imm =>
               Write ("$");
               Write (Node.Imm_Int);
            when A_Reg =>
               Write ("%");
               Print (Node.Reg.all, Width);
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
         (Cond : Condition_Code)
      is
      begin
         case Cond is
            when E => Write ("e");
            when NE => Write ("ne");
            when G => Write ("g");
            when GE => Write ("ge");
            when L => Write ("l");
            when LE => Write ("le");
         end case;
      end Print;

      procedure Print
         (Name : Identifier)
      is
      begin
         Write (To_String (Name));
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
            when A_Cmp =>
               Write ("cmpl ");
               Print (Node.B.all);
               Write (", ");
               Print (Node.A.all);
            when A_Cdq =>
               Write ("cdq");
            when A_Idiv =>
               Write ("idivl ");
               Print (Node.Idiv_Src.all);
            when A_Jmp =>
               Write ("jmp .L");
               Print (Node.Jmp_Label);
            when A_JmpCC =>
               Write ("j");
               Print (Node.JmpCC_Condition);
               Write (" .L");
               Print (Node.JmpCC_Label);
            when A_SetCC =>
               Write ("set");
               Print (Node.SetCC_Condition);
               Write (" ");
               Print (Node.SetCC_Operand.all, Width => RW_8);
            when A_Label =>
               New_Line;
               Write (".L");
               Print (Node.Label);
               Write (":");
               Indent;
            when A_Allocate_Stack =>
               Write ("subq $");
               Write (Long_Integer (Node.Stack_Size));
               Write (", %rsp");
            when A_Deallocate_Stack =>
               Write ("addq $");
               Write (Long_Integer (Node.Stack_Size));
               Write (", %rsp");
            when A_Push =>
               Write ("pushq ");
               Print (Node.Push_Operand.all, Width => RW_64);
            when A_Call =>
               Write ("call ");
               Print (Node.Call_Name);
               Write ("@PLT");
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

      for Def of Node.Function_Definitions loop
         Print (Def.all);
      end loop;

      Indent;
      Write (".section .note.GNU-stack,"""",@progbits");
      New_Line;
      New_Line;

      WACC.IO.Close (File);
   end Emit;

end WACC.Assembly;
