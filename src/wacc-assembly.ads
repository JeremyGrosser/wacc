pragma Style_Checks ("M120");
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

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

   type Operand_Type is (A_Imm, A_Register);
   type Operand_Node
      (Typ : Operand_Type)
   is record
      case Typ is
         when A_Imm =>
            Int : Long_Integer;
         when A_Register =>
            null;
      end case;
   end record;
   type Any_Operand_Node is access Operand_Node;

   type Instruction_Type is (A_Mov, A_Ret);
   type Instruction_Node
      (Typ : Instruction_Type)
   is record
      case Typ is
         when A_Mov =>
            Src, Dst : Any_Operand_Node;
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

   procedure Print
      (Node : Program_Node;
       Filename : String);

end WACC.Assembly;
