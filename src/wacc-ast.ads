with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package WACC.AST
   with Preelaborate
is

   --  program = Program(function_definition)
   --  function_definition = Function(identifier name, statement body)
   --  statement = Return(exp)
   --  exp = Constant(int)

   subtype Identifier is Unbounded_String;

   type Node is tagged null record;

   type Exp_Type is
      (T_Constant);
   type Exp_Node
      (Typ : Exp_Type)
   is new Node with record
      case Typ is
         when T_Constant =>
            Int : Integer;
      end case;
   end record;
   type Any_Exp_Node is not null access Exp_Node'Class;

   type Statement_Type is
      (T_Return);
   type Statement_Node
      (Typ : Statement_Type)
   is new Node with record
      case Typ is
         when T_Return =>
            Exp : Any_Exp_Node;
      end case;
   end record;
   type Any_Statement_Node is not null access Statement_Node'Class;

   type Function_Definition_Node is new Node with record
      Name  : Identifier;
      FBody : Any_Statement_Node;
   end record;

   type Program_Node is new Node with record
      Function_Definition : Function_Definition_Node;
   end record;

end WACC.AST;
