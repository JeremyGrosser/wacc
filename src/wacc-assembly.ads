pragma Style_Checks ("M120");
with Ada.Containers.Vectors;
with WACC.Strings; use WACC.Strings;
with WACC.TACKY;

package WACC.Assembly is

   type Program_Node;
   type Any_Program_Node is access Program_Node;
   type Function_Definition_Node;
   type Any_Function_Definition_Node is access Function_Definition_Node;
   type Instruction_Node;
   type Any_Instruction_Node is access Instruction_Node;
   type Unary_Operator_Node;
   type Any_Unary_Operator_Node is access Unary_Operator_Node;
   type Binary_Operator_Node;
   type Any_Binary_Operator_Node is access Binary_Operator_Node;
   type Operand_Node;
   type Any_Operand_Node is access Operand_Node;
   type Reg_Node;
   type Any_Reg_Node is access Reg_Node;

   Assembly_Error : exception;

   type Stack_Offset is new Integer;

   package Instruction_Node_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Natural,
       Element_Type => Any_Instruction_Node);

   package Function_Definition_Node_Vectors is new Ada.Containers.Vectors
      (Index_Type    => Positive,
       Element_Type  => Any_Function_Definition_Node);

   --  program = Program(function_definition*)
   type Program_Node is record
      Function_Definitions : Function_Definition_Node_Vectors.Vector;
   end record;

   --  function_definition = Function(identifier name, instruction* instructions)
   type Function_Definition_Node is record
      Name : Identifier;
      Instructions : Instruction_Node_Vectors.Vector;
   end record;

   --  cond_code = E | NE | G | GE | L | LE
   type Condition_Code is (E, NE, G, GE, L, LE);

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
   --              | DeallocateStack(int)
   --              | Push(operand)
   --              | Call(identifier)
   --              | Ret
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
       A_Deallocate_Stack,
       A_Push,
       A_Call,
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
         when A_Allocate_Stack | A_Deallocate_Stack =>
            Stack_Size : Natural;
         when A_Push =>
            Push_Operand : Any_Operand_Node;
         when A_Call =>
            Call_Name : Identifier;
         when A_Ret =>
            null;
      end case;
   end record;

   --  unary_operator = Neg | Not
   type Unary_Operator_Type is (A_Neg, A_Not);
   type Unary_Operator_Node
      (Typ : Unary_Operator_Type)
   is null record;

   --  binary_operator = Add | Sub | Mult
   type Binary_Operator_Type is (A_Add, A_Sub, A_Mult);
   type Binary_Operator_Node
      (Typ : Binary_Operator_Type)
   is null record;

   --  operand = Imm(int) | Reg(reg) | Pseudo(identifier) | Stack(int)
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

   --  reg = AX | CX | DX | DI | SI | R8 | R9 | R10 | R11
   type Reg_Node_Type is (A_AX, A_CX, A_DX, A_DI, A_SI, A_R8, A_R9, A_R10, A_R11);
   type Reg_Node
      (Typ : Reg_Node_Type)
   is null record;

   procedure Generate
      (Tree : WACC.TACKY.Program_Node;
       Asm  : out Program_Node);

   procedure Print
      (Node : Program_Node);

   procedure Emit
      (Node : Program_Node;
       Filename : String);

end WACC.Assembly;
