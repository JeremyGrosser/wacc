pragma Style_Checks ("M120");
with WACC.Strings; use WACC.Strings;

package WACC.AST
   with Elaborate_Body
is

   --  program = Program(function_definition)
   --  function_definition = Function(identifier name, block body)
   --  block_item = statement | declaration
   --  block = Block(block_item*)
   --  declaration = Declaration(identifier name, exp? init)
   --  for_init = InitDecl(declaration) | InitExp(exp?)
   --  statement = Return(exp)
   --            | Expression(exp)
   --            | If(exp condition, statement then, statement? else)
   --            | Compound(block)
   --            | Break(identifier label)
   --            | Continue(identifier label)
   --            | While(exp condition, statement body, identifier label)
   --            | DoWhile(statement body, exp condition, identifier label)
   --            | For(for_init init, exp? condition, exp? post, statement body)
   --            | Goto(identifier label)
   --            | Label(identifier name)
   --            | Null
   --  exp = Constant(int)
   --      | Var(identifier)
   --      | Unary(unary_operator, exp)
   --      | Binary(binary_operator, exp, exp)
   --      | Assignment(exp, exp)
   --      | Conditional(exp condition, exp, exp)
   --  unary_operator = Complement | Negate | Not
   --  binary_operator = Add | Subtract | Multiply | Divide | Remainder | And
   --                  | Or | Equal | NotEqual | LessThan | LessOrEqual
   --                  | GreaterThan | GreaterOrEqual

   type Binary_Operator_Type is
      (N_Add,
       N_Subtract,
       N_Multiply,
       N_Divide,
       N_Remainder,
       N_And,
       N_Or,
       N_Equal,
       N_Not_Equal,
       N_Less_Than,
       N_Less_Or_Equal,
       N_Greater_Than,
       N_Greater_Or_Equal);
   type Binary_Operator_Node
      (Typ : Binary_Operator_Type)
   is null record;
   type Any_Binary_Operator_Node is access Binary_Operator_Node;

   type Unary_Operator_Type is
      (N_Complement,
       N_Negate,
       N_Not);
   type Unary_Operator_Node
      (Typ : Unary_Operator_Type)
   is null record;
   type Any_Unary_Operator_Node is access Unary_Operator_Node;

   type Exp_Type is
      (N_Constant,
       N_Var,
       N_Unary,
       N_Binary,
       N_Assignment,
       N_Conditional);

   type Exp_Node;
   type Any_Exp_Node is access Exp_Node;

   type Exp_Node
      (Typ : Exp_Type)
   is record
      case Typ is
         when N_Constant =>
            Int : Long_Integer;
         when N_Var =>
            Name : Identifier;
         when N_Unary =>
            Unary_Operator : Any_Unary_Operator_Node;
            Exp : Any_Exp_Node;
         when N_Binary =>
            Binary_Operator : Any_Binary_Operator_Node;
            Left, Right : Any_Exp_Node;
         when N_Assignment =>
            Assign_Left, Assign_Right : Any_Exp_Node;
         when N_Conditional =>
            Condition, If_True, If_False : Any_Exp_Node;
      end case;
   end record;

   type Declaration_Node is record
      Name : Identifier;
      Init : Any_Exp_Node;
   end record;
   type Any_Declaration_Node is access Declaration_Node;

   type For_Init_Type is
      (N_Init_Declaration,
       N_Init_Expression);
   type For_Init_Node
      (Typ : For_Init_Type)
   is record
      case Typ is
         when N_Init_Declaration =>
            Decl : Any_Declaration_Node;
         when N_Init_Expression =>
            Exp : Any_Exp_Node;
      end case;
   end record;
   type Any_For_Init_Node is access For_Init_Node;

   type Block_Node;
   type Any_Block_Node is access Block_Node;

   type Statement_Type is
      (N_Return,
       N_Expression,
       N_If,
       N_Compound,
       N_Break,
       N_Continue,
       N_While,
       N_DoWhile,
       N_For,
       N_Goto,
       N_Label,
       N_Null);

   type Statement_Node;
   type Any_Statement_Node is access Statement_Node;

   type Statement_Node
      (Typ : Statement_Type)
   is record
      case Typ is
         when N_Return | N_Expression =>
            Exp : Any_Exp_Node;
         when N_If =>
            Condition : Any_Exp_Node;
            If_True   : Any_Statement_Node;
            If_False  : Any_Statement_Node;
         when N_Compound =>
            Block : Any_Block_Node;
         when N_While | N_DoWhile =>
            While_Label : Identifier;
            While_Condition : Any_Exp_Node;
            While_Body : Any_Statement_Node;
         when N_For =>
            For_Label : Identifier;
            For_Init : Any_For_Init_Node;
            For_Condition : Any_Exp_Node;
            For_Post : Any_Exp_Node;
            For_Body : Any_Statement_Node;
         when N_Break | N_Continue | N_Goto | N_Label =>
            Label : Identifier;
         when N_Null =>
            null;
      end case;
   end record;

   type Block_Item_Node;
   type Any_Block_Item_Node is access Block_Item_Node;

   type Block_Item_Type is
      (N_Statement,
       N_Declaration);
   type Block_Item_Node
      (Typ : Block_Item_Type)
   is record
      Next : Any_Block_Item_Node := null;
      case Typ is
         when N_Statement =>
            Stmt : Any_Statement_Node;
         when N_Declaration =>
            Decl : Any_Declaration_Node;
      end case;
   end record;

   type Block_Node is record
      Head : Any_Block_Item_Node;
   end record;

   type Function_Definition_Node is record
      Name  : Identifier;
      FBody : Any_Block_Node;
   end record;

   type Program_Node is record
      Function_Definition : Function_Definition_Node;
   end record;

   procedure Print
      (This : Program_Node);

end WACC.AST;
