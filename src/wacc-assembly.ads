pragma Style_Checks ("M120");
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package WACC.Assembly is

   --  program = Program(function_definition)
   --  function_definition = Function(identifier name, instruction* instructions)
   --  instruction = Mov(operand src, operand dst) | Ret
   --  operand = Imm(int) | Register

   subtype Identifier is Unbounded_String;

   type Operand_Type is (Imm, Register);
   type Operand
      (Typ : Operand_Type)
   is record
      case Typ is
         when Imm =>
            Int : Long_Integer;
         when Register =>
            null;
      end case;
   end record;
   type Any_Operand is access Operand;

   type Instruction_Type is (Mov, Ret);

   type Instruction
      (Typ : Instruction_Type)
   is record
      case Typ is
         when Mov =>
            Src, Dst : Any_Operand;
         when Ret =>
            null;
      end case;
   end record;
   type Any_Instruction is access Instruction;

   package Instruction_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Natural,
       Element_Type => Any_Instruction);

   type Function_Definition_Node is record
      Name : Identifier;
      Instructions : Instruction_Vectors.Vector;
   end record;

   type Program_Node is record
      Function_Definition : Function_Definition_Node;
   end record;

   procedure Print
      (Node : Program_Node;
       Filename : String);

end WACC.Assembly;
