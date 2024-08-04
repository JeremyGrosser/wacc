pragma Style_Checks ("M120");
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with WACC.AST;

package WACC.TACKY is

   --  program = Program(function_definition)
   --  function_definition = Function(identifier, 1 instruction* body)
   --  instruction = Return(val)
   --              | Unary(unary_operator, val src, val dst)
   --              | Binary(binary_operator, val src1, val src2, val dst)
   --  val = Constant(int) | Var(identifier)
   --  unary_operator = Complement | Negate
   --  binary_operator = Add | Subtract | Multiply | Divide | Remainder | And | Or | Xor | Left_Shift | Right_Shift

   subtype Identifier is Unbounded_String;

   type Binary_Operator_Type is
      (TA_Add,
       TA_Subtract,
       TA_Multiply,
       TA_Divide,
       TA_Remainder,
       TA_And,
       TA_Or,
       TA_Xor,
       TA_Left_Shift,
       TA_Right_Shift);
   type Binary_Operator_Node
      (Typ : Binary_Operator_Type)
   is null record;
   type Any_Binary_Operator_Node is access Binary_Operator_Node;

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
       TA_Unary,
       TA_Binary);
   type Instruction_Node
      (Typ : Instruction_Node_Type)
   is record
      case Typ is
         when TA_Return =>
            Val : Any_Val_Node;
         when TA_Unary =>
            Unary_Operator : Any_Unary_Operator_Node;
            Unop_Src, Unop_Dst : Any_Val_Node;
         when TA_Binary =>
            Binary_Operator : Any_Binary_Operator_Node;
            Binop_Src1, Binop_Src2, Binop_Dst : Any_Val_Node;
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
