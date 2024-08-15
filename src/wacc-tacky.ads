with Ada.Containers.Vectors;
with WACC.Strings; use WACC.Strings;
with WACC.AST;

package WACC.TACKY is

   --  program = Program(function_definition)
   --  function_definition = Function(identifier, 1 instruction* body)
   --  instruction = Return(val)
   --              | Unary(unary_operator, val src, val dst)
   --              | Binary(binary_operator, val src1, val src2, val dst)
   --              | Copy(val src, val dst)
   --              | Jump(identifier target)
   --              | JumpIfZero(val condition, identifier target)
   --              | JumpIfNotZero(val condition, identifier target)
   --              | Label(identifier)
   --  val = Constant(int) | Var(identifier)
   --  unary_operator = Complement | Negate | Not
   --  binary_operator = Add | Subtract | Multiply | Divide | Remainder | Equal
   --                  | NotEqual | LessThan | LessOrEqual | GreaterThan
   --                  | GreaterOrEqual

   type Binary_Operator_Type is
      (TA_Add,
       TA_Subtract,
       TA_Multiply,
       TA_Divide,
       TA_Remainder,
       TA_Equal,
       TA_Not_Equal,
       TA_Less_Than,
       TA_Less_Or_Equal,
       TA_Greater_Than,
       TA_Greater_Or_Equal);
   type Binary_Operator_Node
      (Typ : Binary_Operator_Type)
   is null record;
   type Any_Binary_Operator_Node is access Binary_Operator_Node;

   type Unary_Operator_Type is
      (TA_Complement,
       TA_Negate,
       TA_Not);
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
       TA_Binary,
       TA_Copy,
       TA_Jump,
       TA_Jump_If_Zero,
       TA_Jump_If_Not_Zero,
       TA_Label);
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
         when TA_Copy =>
            Copy_Src, Copy_Dst : Any_Val_Node;
         when TA_Jump =>
            J_Target : Identifier;
         when TA_Jump_If_Zero =>
            JZ_Condition : Any_Val_Node;
            JZ_Target : Identifier;
         when TA_Jump_If_Not_Zero =>
            JNZ_Condition : Any_Val_Node;
            JNZ_Target : Identifier;
         when TA_Label =>
            Label : Identifier;
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
