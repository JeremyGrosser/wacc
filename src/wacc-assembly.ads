pragma Style_Checks ("M120");
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Ada.Containers.Vectors;
with WACC.TACKY;

package WACC.Assembly is

   --  program = Program(function_definition)
   --  function_definition = Function(identifier name, instruction* instructions)
   --  instruction = Mov(operand src, operand dst)
   --              | Unary(unary_operator, operand)
   --              | AllocateStack(int)
   --              | Ret
   --  unary_operator = Neg | Not
   --  operand = Imm(int) | Reg(reg) | Pseudo(identifier) | Stack(int)
   --  reg = AX | R10

   subtype Identifier is Unbounded_String;
   subtype Stack_Offset is Integer range Integer'First .. 4;

   type Reg_Node_Type is (A_AX, A_R10);
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
   procedure Free is new Ada.Unchecked_Deallocation (Operand_Node, Any_Operand_Node);

   type Unary_Operator_Type is (A_Neg, A_Not);
   type Unary_Operator_Node
      (Typ : Unary_Operator_Type)
   is null record;
   type Any_Unary_Operator_Node is access Unary_Operator_Node;

   type Instruction_Type is (A_Mov, A_Unary, A_Allocate_Stack, A_Ret);
   type Instruction_Node
      (Typ : Instruction_Type)
   is record
      case Typ is
         when A_Mov =>
            Src, Dst : Any_Operand_Node;
         when A_Unary =>
            Unary_Operator : Any_Unary_Operator_Node;
            Operand : Any_Operand_Node;
         when A_Allocate_Stack =>
            Int : Long_Integer;
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

   procedure Generate
      (Tree : WACC.TACKY.Program_Node;
       Asm  : out Program_Node);

   procedure Print
      (Node : Program_Node);

end WACC.Assembly;
