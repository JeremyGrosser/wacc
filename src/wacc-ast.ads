pragma Style_Checks ("M120");
with WACC.Strings; use WACC.Strings;
with Ada.Containers.Vectors;

package WACC.AST
   with Elaborate_Body
is
   --  This is a twisty maze of dependent types. Forward declare almost everything.
   type Declaration_Node;
   type Any_Declaration_Node is access Declaration_Node;
   type Function_Declaration_Node;
   type Any_Function_Declaration_Node is access Function_Declaration_Node;
   type Variable_Declaration_Node;
   type Any_Variable_Declaration_Node is access Variable_Declaration_Node;
   type Storage_Class_Node;
   type Any_Storage_Class_Node is access Storage_Class_Node;
   type Block_Item_Node;
   type Any_Block_Item_Node is access Block_Item_Node;
   type Block_Node;
   type Any_Block_Node is access Block_Node;
   type For_Init_Node;
   type Any_For_Init_Node is access For_Init_Node;
   type Statement_Node;
   type Any_Statement_Node is access Statement_Node;
   type Exp_Node;
   type Any_Exp_Node is access Exp_Node;
   type Unary_Operator_Node;
   type Any_Unary_Operator_Node is access Unary_Operator_Node;
   type Binary_Operator_Node;
   type Any_Binary_Operator_Node is access Binary_Operator_Node;

   package Identifier_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Identifier);

   package Exp_Node_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Natural,
       Element_Type => Any_Exp_Node);

   package Declaration_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Any_Declaration_Node);

   --  program = Program(declaration*)
   type Program_Node is record
      Declarations : Declaration_Vectors.Vector;
   end record;

   --  declaration = FunDecl(function_declaration) | VarDecl(variable_declaration)
   type Declaration_Type is (N_FunDecl, N_VarDecl);
   type Declaration_Node
      (Typ : Declaration_Type)
   is record
      case Typ is
         when N_FunDecl =>
            Function_Declaration : Any_Function_Declaration_Node;
         when N_VarDecl =>
            Variable_Declaration : Any_Variable_Declaration_Node;
      end case;
   end record;

   --  variable_declaration = (identifier name, exp? int, storage_class?)
   type Variable_Declaration_Node is record
      Name : Identifier;
      Init : Any_Exp_Node;
      Storage_Class : Any_Storage_Class_Node;
   end record;

   --  function_declaration = (identifier name, identifier* params, block? body, storage_class?)
   type Function_Declaration_Node is record
      Name   : Identifier;
      Params : Identifier_Vectors.Vector;
      FBody  : Any_Block_Node;
      Storage_Class : Any_Storage_Class_Node;
   end record;

   --  storage_class = Static | Extern
   type Storage_Class_Type is (Static, Extern);
   type Storage_Class_Node
      (Typ : Storage_Class_Type)
   is null record;

   --  block_item = statement | declaration
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

   --  block = Block(block_item*)
   type Block_Node is record
      Head : Any_Block_Item_Node;
   end record;

   --  for_init = InitDecl(variable_declaration) | InitExp(exp?)
   type For_Init_Type is
      (N_Init_Declaration,
       N_Init_Expression);
   type For_Init_Node
      (Typ : For_Init_Type)
   is record
      case Typ is
         when N_Init_Declaration =>
            Decl : Any_Variable_Declaration_Node;
         when N_Init_Expression =>
            Exp : Any_Exp_Node;
      end case;
   end record;

   --  statement = Return(exp)
   --            | Expression(exp)
   --            | If(exp condition, statement then, statement? else)
   --            | Compound(block)
   --            | Break(identifier label)
   --            | Continue(identifier label)
   --            | While(exp condition, statement body, identifier label)
   --            | DoWhile(statement body, exp condition, identifier label)
   --            | For(for_init init, exp? condition, exp? post, statement body)
   --            | Null
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
       N_Null);
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
         when N_Break | N_Continue =>
            Label : Identifier;
         when N_Null =>
            null;
      end case;
   end record;

   --  exp = Constant(int)
   --      | Var(identifier)
   --      | Unary(unary_operator, exp)
   --      | Binary(binary_operator, exp, exp)
   --      | Assignment(exp, exp)
   --      | Conditional(exp condition, exp, exp)
   --      | FunctionCall(identifier, exp* args)
   type Exp_Type is
      (N_Constant,
       N_Var,
       N_Unary,
       N_Binary,
       N_Assignment,
       N_Conditional,
       N_Function_Call);
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
         when N_Function_Call =>
            Function_Name : Identifier;
            Args : Exp_Node_Vectors.Vector := Exp_Node_Vectors.Empty_Vector;
      end case;
   end record;

   --  unary_operator = Complement | Negate | Not
   type Unary_Operator_Type is
      (N_Complement,
       N_Negate,
       N_Not);
   type Unary_Operator_Node
      (Typ : Unary_Operator_Type)
   is null record;

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

   procedure Print
      (This : Program_Node);

end WACC.AST;
