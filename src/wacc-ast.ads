with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package WACC.AST
   with Elaborate_Body
is

   --  program = Program(function_definition)
   --  function_definition = Function(identifier name, statement body)
   --  statement = Return(exp)
   --  exp = Constant(int) | Unary(unary_operator, exp)
   --  unary_operator = Complement | Negate

   subtype Identifier is Unbounded_String;

   type Unary_Operator_Type is
      (N_Complement,
       N_Negate);
   type Unary_Operator_Node
      (Typ : Unary_Operator_Type)
   is null record;
   type Any_Unary_Operator_Node is access Unary_Operator_Node;

   type Exp_Type is
      (N_Constant,
       N_Unary);

   type Exp_Node;
   type Any_Exp_Node is access Exp_Node;

   type Exp_Node
      (Typ : Exp_Type)
   is record
      case Typ is
         when N_Constant =>
            Int : Long_Integer;
         when N_Unary =>
            Unary_Operator : Any_Unary_Operator_Node;
            Exp : Any_Exp_Node;
      end case;
   end record;

   type Statement_Type is
      (N_Return);
   type Statement_Node
      (Typ : Statement_Type)
   is record
      case Typ is
         when N_Return =>
            Exp : Any_Exp_Node;
      end case;
   end record;
   type Any_Statement_Node is access Statement_Node;

   type Function_Definition_Node is record
      Name  : Identifier;
      FBody : Any_Statement_Node;
   end record;

   type Program_Node is record
      Function_Definition : Function_Definition_Node;
   end record;

   procedure Print
      (This : Program_Node);

end WACC.AST;
