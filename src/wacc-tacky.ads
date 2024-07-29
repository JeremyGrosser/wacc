with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with WACC.AST;

package WACC.TACKY is

   --  program = Program(function_definition)
   --  function_definition = Function(identifier, 1 instruction* body)
   --  instruction = Return(val) | Unary(unary_operator, val src, val dst)
   --  val = Constant(int) | Var(identifier)
   --  unary_operator = Complement | Negate

   subtype Identifier is Unbounded_String;

   type Unary_Operator_Type is
      (TA_Complement,
       TA_Negate);
   type Unary_Operator_Node
      (Typ : Unary_Operator_Type)
   is null record;
   type Any_Unary_Operator_Node is access Unary_Operator_Node;

   type Val_Node_Type is
      (TA_Constant,
       TA_Var);
   type Val_Node
      (Typ : Val_Node_Type)
   is record
      case Typ is
         when TA_Constant =>
            Int : Long_Integer;
         when TA_Var =>
            Name : Identifier;
      end case;
   end record;
   type Any_Val_Node is access Val_Node;

   type Instruction_Node_Type is
      (TA_Return,
       TA_Unary);
   type Instruction_Node
      (Typ : Instruction_Node_Type)
   is record
      case Typ is
         when TA_Return =>
            Val : Any_Val_Node;
         when TA_Unary =>
            Unary_Operator : Any_Unary_Operator_Node;
            Src, Dst : Any_Val_Node;
      end case;
   end record;
   type Any_Instruction_Node is access Instruction_Node;

   package Instruction_Node_Vectors is new Ada.Containers.Vectors
      (Positive, Any_Instruction_Node);

   type Function_Definition_Node is record
      Name  : Identifier;
      FBody : Instruction_Node_Vectors.Vector;
   end record;

   type Program_Node is record
      Function_Definition : Function_Definition_Node;
   end record;

   procedure Generate
      (Tree : WACC.AST.Program_Node;
       Node : out Program_Node);

   procedure Print
      (Node : Program_Node);

end WACC.TACKY;
