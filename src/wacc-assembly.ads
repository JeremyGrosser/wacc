pragma Style_Checks ("M120");
with Ada.Containers.Vectors;
with WACC.Strings; use WACC.Strings;
with WACC.TACKY;

package WACC.Assembly is

   --  program = Program(function_definition)
   --  function_definition = Function(identifier name, instruction* instructions)
   --  instruction = Mov(operand src, operand dst)
   --              | Unary(unary_operator, operand)
   --              | Binary(binary_operator, operand, operand)
   --              | Cmp(operand, operand)
   --              | Idiv(operand)
   --              | Cdq
   --              | Jmp(identifier)
   --              | JmpCC(cond_code, identifier)
   --              | SetCC(cond_code, identifier)
   --              | Label(identifier)
   --              | AllocateStack(int)
   --              | Ret
   --  unary_operator = Neg | Not
   --  binary_operator = Add | Sub | Mult
   --  operand = Imm(int) | Reg(reg) | Pseudo(identifier) | Stack(int)
   --  cond_code = E | NE | G | GE | L | LE
   --  reg = AX | DX | R10 | R11

   type Stack_Offset is new Integer range Integer'First .. -4;

   type Reg_Node_Type is (A_AX, A_DX, A_R10, A_R11);
   type Reg_Node
      (Typ : Reg_Node_Type)
   is null record;
   type Any_Reg_Node is access Reg_Node;

   type Operand_Type is (A_Imm, A_Reg, A_Pseudo, A_Stack);
   type Operand_Node
      (Typ : Operand_Type)
   is record
      case Typ is
         when A_Imm =>
            Imm_Int : Long_Integer;
         when A_Reg =>
            Reg : Any_Reg_Node;
         when A_Pseudo =>
            Name : Identifier;
         when A_Stack =>
            Stack_Int : Stack_Offset;
      end case;
   end record;
   type Any_Operand_Node is access Operand_Node;

   type Unary_Operator_Type is (A_Neg, A_Not);
   type Unary_Operator_Node
      (Typ : Unary_Operator_Type)
   is null record;
   type Any_Unary_Operator_Node is access Unary_Operator_Node;

   type Binary_Operator_Type is (A_Add, A_Sub, A_Mult);
   type Binary_Operator_Node
      (Typ : Binary_Operator_Type)
   is null record;
   type Any_Binary_Operator_Node is access Binary_Operator_Node;

   type Condition_Code is (E, NE, G, GE, L, LE);

   type Instruction_Type is
      (A_Mov,
       A_Unary,
       A_Binary,
       A_Cmp,
       A_Idiv,
       A_Cdq,
       A_Jmp,
       A_JmpCC,
       A_SetCC,
       A_Label,
       A_Allocate_Stack,
       A_Ret);
   type Instruction_Node
      (Typ : Instruction_Type)
   is record
      case Typ is
         when A_Mov =>
            Src, Dst : Any_Operand_Node;
         when A_Unary =>
            Unary_Operator : Any_Unary_Operator_Node;
            Unary_Operand : Any_Operand_Node;
         when A_Binary =>
            Binary_Operator : Any_Binary_Operator_Node;
            Binary_Src, Binary_Dst : Any_Operand_Node;
         when A_Cmp =>
            A, B : Any_Operand_Node;
         when A_Idiv =>
            Idiv_Src : Any_Operand_Node;
         when A_Cdq =>
            null;
         when A_Jmp =>
            Jmp_Label : Identifier;
         when A_JmpCC =>
            JmpCC_Condition : Condition_Code;
            JmpCC_Label : Identifier;
         when A_SetCC =>
            SetCC_Condition : Condition_Code;
            SetCC_Operand : Any_Operand_Node;
         when A_Label =>
            Label : Identifier;
         when A_Allocate_Stack =>
            Stack_Size : Natural;
         when A_Ret =>
            null;
      end case;
   end record;
   type Any_Instruction_Node is access Instruction_Node;

   package Instruction_Node_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Natural,
       Element_Type => Any_Instruction_Node);

   type Function_Definition_Node is record
      Name : Identifier;
      Instructions : Instruction_Node_Vectors.Vector;
   end record;

   type Program_Node is record
      Function_Definition : Function_Definition_Node;
   end record;

   Assembly_Error : exception;

   procedure Generate
      (Tree : WACC.TACKY.Program_Node;
       Asm  : out Program_Node);

   procedure Print
      (Node : Program_Node);

   procedure Emit
      (Node : Program_Node;
       Filename : String);

end WACC.Assembly;
