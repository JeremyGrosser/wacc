pragma Style_Checks ("M120");
pragma Ada_2022;
with WACC.Strings; use WACC.Strings;
with WACC.Symbols;
with Ada.Containers.Hashed_Maps;

package body WACC.Semantic_Analysis is

   Symbols : WACC.Symbols.Table;

   type Map_Entry is record
      New_Name             : Identifier;
      From_Current_Scope   : Boolean;
      Has_Linkage          : Boolean;
   end record;

   package Identifier_Entry_Maps is new Ada.Containers.Hashed_Maps
      (Key_Type         => Identifier,
       Element_Type     => Map_Entry,
       Hash             => WACC.Strings.Hash,
       Equivalent_Keys  => WACC.Strings."=");
   use Identifier_Entry_Maps;

   subtype Identifier_Map is Identifier_Entry_Maps.Map;

   procedure Copy_Identifier_Map
      (From : Identifier_Map;
       To   : out Identifier_Map);
   procedure Resolve_Block
      (Tree : in out WACC.AST.Block_Node;
       Vars : in out Identifier_Map);
   procedure Resolve_Expression
      (Exp  : in out WACC.AST.Exp_Node;
       Vars : Identifier_Map);
   procedure Resolve_Param
      (Param : in out Identifier;
       Vars  : in out Identifier_Map);
   procedure Resolve_Variable_Declaration
      (Decl : in out WACC.AST.Variable_Declaration_Node;
       Vars : in out Identifier_Map);
   procedure Resolve_Function_Declaration
      (Decl : in out WACC.AST.Function_Declaration_Node;
       Vars : in out Identifier_Map);
   procedure Resolve_Declaration
      (Decl : in out WACC.AST.Declaration_Node;
       Vars : in out Identifier_Map);
   procedure Resolve_Optional_Exp
      (Tree : WACC.AST.Any_Exp_Node;
       Vars : Identifier_Map);
   procedure Resolve_For_Init
      (Tree : in out WACC.AST.For_Init_Node;
       Vars : in out Identifier_Map);
   procedure Resolve_Statement
      (Tree : in out WACC.AST.Statement_Node;
       Vars : Identifier_Map);
   procedure Resolve_Block_Item
      (Tree : in out WACC.AST.Block_Item_Node;
       Vars : in out Identifier_Map);
   procedure Label_Statement
      (Tree  : in out WACC.AST.Statement_Node;
       Label : in out Identifier);
   procedure Label_Block_Item
      (Tree  : in out WACC.AST.Block_Item_Node;
       Label : in out Identifier);
   procedure Typecheck_Exp
      (Exp : in out WACC.AST.Exp_Node);
   procedure Typecheck_Optional_Exp
      (Tree : WACC.AST.Any_Exp_Node);
   procedure Typecheck_Variable_Declaration
      (Decl : in out WACC.AST.Variable_Declaration_Node);
   procedure Typecheck_Function_Declaration
      (Decl : in out WACC.AST.Function_Declaration_Node);
   procedure Typecheck_Declaration
      (Tree : in out WACC.AST.Declaration_Node);
   procedure Typecheck_Statement
      (Tree : in out WACC.AST.Statement_Node);
   procedure Typecheck_For_Init
      (Tree : in out WACC.AST.For_Init_Node);
   procedure Typecheck_Block_Item
      (Tree : in out WACC.AST.Block_Item_Node);
   procedure Typecheck_Block
      (Tree : in out WACC.AST.Block_Node);

   --  Copy a map, setting From_Current_Scope := False; for all elements.
   procedure Copy_Identifier_Map
      (From : Identifier_Map;
       To   : out Identifier_Map)
   is
   begin
      for Cursor in Iterate (From) loop
         declare
            Item : Map_Entry := Element (Cursor);
         begin
            Item.From_Current_Scope := False;
            Include (To, Key (Cursor), Item);
         end;
      end loop;
   end Copy_Identifier_Map;

   procedure Resolve_Expression
      (Exp  : in out WACC.AST.Exp_Node;
       Vars : Identifier_Map)
   is
      use type WACC.AST.Exp_Type;
   begin
      case Exp.Typ is
         when WACC.AST.N_Constant =>
            null;
         when WACC.AST.N_Var =>
            if Contains (Vars, Exp.Name) then
               Exp.Name := Element (Vars, Exp.Name).New_Name;
            else
               raise Semantic_Error with "Undeclared variable in initializer: " & To_String (Exp.Name);
            end if;
         when WACC.AST.N_Unary =>
            Resolve_Expression (Exp.Exp.all, Vars);
         when WACC.AST.N_Binary =>
            Resolve_Expression (Exp.Left.all, Vars);
            Resolve_Expression (Exp.Right.all, Vars);
         when WACC.AST.N_Assignment =>
            if Exp.Assign_Left.Typ /= WACC.AST.N_Var then
               raise Semantic_Error with "Invalid lvalue!";
            else
               Resolve_Expression (Exp.Assign_Left.all, Vars);
               Resolve_Expression (Exp.Assign_Right.all, Vars);
            end if;
         when WACC.AST.N_Conditional =>
            Resolve_Expression (Exp.Condition.all, Vars);
            Resolve_Expression (Exp.If_True.all, Vars);
            Resolve_Expression (Exp.If_False.all, Vars);
         when WACC.AST.N_Function_Call =>
            if Contains (Vars, Exp.Function_Name) then
               Exp.Function_Name := Element (Vars, Exp.Function_Name).New_Name;
               for Arg of Exp.Args loop
                  Resolve_Expression (Arg.all, Vars);
               end loop;
            else
               raise Semantic_Error with "Call to undeclared function: " & To_String (Exp.Function_Name);
            end if;
      end case;
   end Resolve_Expression;

   procedure Resolve_Param
      (Param : in out Identifier;
       Vars  : in out Identifier_Map)
   is
   begin
      if Contains (Vars, Param) and then Element (Vars, Param).From_Current_Scope then
         raise Semantic_Error with "Duplicate param """ & To_String (Param) & """ has already been declared!";
      end if;

      declare
         Unique : constant Identifier := Make_Identifier
            (Prefix => To_String (Param) & ".");
      begin
         Include (Vars, Param, Map_Entry'
            (New_Name            => Unique,
             From_Current_Scope  => True,
             Has_Linkage         => False));
         Param := Unique;
      end;
   end Resolve_Param;

   procedure Resolve_Variable_Declaration
      (Decl : in out WACC.AST.Variable_Declaration_Node;
       Vars : in out Identifier_Map)
   is
      use type WACC.AST.Any_Exp_Node;
   begin
      Resolve_Param (Decl.Name, Vars);
      if Decl.Init /= null then
         Resolve_Expression (Decl.Init.all, Vars);
      end if;
   end Resolve_Variable_Declaration;

   procedure Resolve_Function_Declaration
      (Decl : in out WACC.AST.Function_Declaration_Node;
       Vars : in out Identifier_Map)
   is
      use type WACC.AST.Any_Block_Node;
      Prev_Entry : Map_Entry;
   begin
      if Contains (Vars, Decl.Name) then
         Prev_Entry := Element (Vars, Decl.Name);
         if Prev_Entry.From_Current_Scope and then not Prev_Entry.Has_Linkage then
            raise Semantic_Error with "Duplicate declaration";
         end if;
      end if;

      Include (Vars, Decl.Name, Map_Entry'
         (New_Name            => Decl.Name,
          From_Current_Scope  => True,
          Has_Linkage         => True));

      declare
         Inner_Map : Identifier_Map;
      begin
         Copy_Identifier_Map (Vars, Inner_Map);
         for Param of Decl.Params loop
            Resolve_Param (Param, Inner_Map);
         end loop;

         if Decl.FBody /= null then
            Resolve_Block (Decl.FBody.all, Inner_Map);
         end if;
      end;
   end Resolve_Function_Declaration;

   procedure Resolve_Declaration
      (Decl : in out WACC.AST.Declaration_Node;
       Vars : in out Identifier_Map)
   is
      use type WACC.AST.Any_Block_Node;
   begin
      case Decl.Typ is
         when WACC.AST.N_FunDecl =>
            if Decl.Function_Declaration.FBody /= null then
               raise Semantic_Error with "Nested function definitions not allowed.";
            else
               Resolve_Function_Declaration (Decl.Function_Declaration.all, Vars);
            end if;
         when WACC.AST.N_VarDecl =>
            Resolve_Variable_Declaration (Decl.Variable_Declaration.all, Vars);
      end case;
   end Resolve_Declaration;

   procedure Resolve_Optional_Exp
      (Tree : WACC.AST.Any_Exp_Node;
       Vars : Identifier_Map)
   is
      use type WACC.AST.Any_Exp_Node;
   begin
      if Tree /= null then
         Resolve_Expression (Tree.all, Vars);
      end if;
   end Resolve_Optional_Exp;

   procedure Resolve_For_Init
      (Tree : in out WACC.AST.For_Init_Node;
       Vars : in out Identifier_Map)
   is
   begin
      case Tree.Typ is
         when WACC.AST.N_Init_Expression =>
            Resolve_Optional_Exp (Tree.Exp, Vars);
         when WACC.AST.N_Init_Declaration =>
            Resolve_Variable_Declaration (Tree.Decl.all, Vars);
      end case;
   end Resolve_For_Init;

   procedure Resolve_Statement
      (Tree : in out WACC.AST.Statement_Node;
       Vars : Identifier_Map)
   is
      use type WACC.AST.Any_Statement_Node;
   begin
      case Tree.Typ is
         when WACC.AST.N_Return | WACC.AST.N_Expression =>
            Resolve_Expression (Tree.Exp.all, Vars);
         when WACC.AST.N_If =>
            Resolve_Expression (Tree.Condition.all, Vars);
            Resolve_Statement (Tree.If_True.all, Vars);
            if Tree.If_False /= null then
               Resolve_Statement (Tree.If_False.all, Vars);
            end if;
         when WACC.AST.N_Compound =>
            declare
               New_Identifier_Map : Identifier_Map := Empty_Map;
            begin
               Copy_Identifier_Map (Vars, New_Identifier_Map);
               Resolve_Block (Tree.Block.all, New_Identifier_Map);
            end;
         when WACC.AST.N_For =>
            declare
               New_Identifier_Map : Identifier_Map := Empty_Map;
            begin
               Copy_Identifier_Map (Vars, New_Identifier_Map);
               Resolve_For_Init (Tree.For_Init.all, New_Identifier_Map);
               Resolve_Optional_Exp (Tree.For_Condition, New_Identifier_Map);
               Resolve_Optional_Exp (Tree.For_Post, New_Identifier_Map);
               Resolve_Statement (Tree.For_Body.all, New_Identifier_Map);
            end;
         when WACC.AST.N_While | WACC.AST.N_DoWhile =>
            Resolve_Expression (Tree.While_Condition.all, Vars);
            Resolve_Statement (Tree.While_Body.all, Vars);
         when WACC.AST.N_Null | WACC.AST.N_Break | WACC.AST.N_Continue =>
            null;
      end case;
   end Resolve_Statement;

   procedure Resolve_Block_Item
      (Tree : in out WACC.AST.Block_Item_Node;
       Vars : in out Identifier_Map)
   is
   begin
      case Tree.Typ is
         when WACC.AST.N_Statement =>
            Resolve_Statement (Tree.Stmt.all, Vars);
         when WACC.AST.N_Declaration =>
            Resolve_Declaration (Tree.Decl.all, Vars);
      end case;
   end Resolve_Block_Item;

   procedure Resolve_Block
      (Tree : in out WACC.AST.Block_Node;
       Vars : in out Identifier_Map)
   is
      use type WACC.AST.Any_Block_Item_Node;
      Node : WACC.AST.Any_Block_Item_Node := Tree.Head;
   begin
      while Node /= null loop
         Resolve_Block_Item (Node.all, Vars);
         Node := Node.Next;
      end loop;
   end Resolve_Block;

   procedure Label_Block
      (Tree  : in out WACC.AST.Block_Node;
       Label : in out Identifier);

   procedure Label_Statement
      (Tree  : in out WACC.AST.Statement_Node;
       Label : in out Identifier)
   is
      use type WACC.AST.Any_Statement_Node;
   begin
      case Tree.Typ is
         when WACC.AST.N_Break =>
            if Label = Null_Identifier then
               raise Semantic_Error with "break statement outside of loop";
            else
               Tree.Label := Label;
            end if;
         when WACC.AST.N_Continue =>
            if Label = Null_Identifier then
               raise Semantic_Error with "continue statement outside of loop";
            else
               Tree.Label := Label;
            end if;
         when WACC.AST.N_While | WACC.AST.N_DoWhile =>
            Tree.While_Label := Make_Identifier ("loop.");
            Label_Statement (Tree.While_Body.all, Tree.While_Label);
         when WACC.AST.N_For =>
            Tree.For_Label := Make_Identifier ("loop.");
            Label_Statement (Tree.For_Body.all, Tree.For_Label);
         when WACC.AST.N_If =>
            Label_Statement (Tree.If_True.all, Label);
            if Tree.If_False /= null then
               Label_Statement (Tree.If_False.all, Label);
            end if;
         when WACC.AST.N_Compound =>
            Label_Block (Tree.Block.all, Label);
         when WACC.AST.N_Return | WACC.AST.N_Expression | WACC.AST.N_Null =>
            null;
      end case;
   end Label_Statement;

   procedure Label_Block_Item
      (Tree  : in out WACC.AST.Block_Item_Node;
       Label : in out Identifier)
   is
      use type WACC.AST.Block_Item_Type;
   begin
      case Tree.Typ is
         when WACC.AST.N_Statement =>
            Label_Statement (Tree.Stmt.all, Label);
         when WACC.AST.N_Declaration =>
            null;
      end case;
   end Label_Block_Item;

   procedure Label_Block
      (Tree  : in out WACC.AST.Block_Node;
       Label : in out Identifier)
   is
      use type WACC.AST.Any_Block_Item_Node;
      Node : WACC.AST.Any_Block_Item_Node := Tree.Head;
   begin
      while Node /= null loop
         Label_Block_Item (Node.all, Label);
         Node := Node.Next;
      end loop;
   end Label_Block;

   procedure Typecheck_Exp
      (Exp : in out WACC.AST.Exp_Node)
   is
      use WACC.Symbols;
      use WACC.AST.Exp_Node_Vectors;
      use type WACC.AST.Any_Exp_Node;
      Symbol : Any_Type_Node;
   begin
      case Exp.Typ is
         when WACC.AST.N_Constant =>
            null;
         when WACC.AST.N_Var =>
            Symbol := WACC.Symbols.Get (Symbols, Exp.Name);
            if Symbol.Typ /= S_Int then
               raise Semantic_Error with "Function name used as variable";
            end if;
         when WACC.AST.N_Unary =>
            Typecheck_Exp (Exp.Exp.all);
         when WACC.AST.N_Binary =>
            Typecheck_Exp (Exp.Left.all);
            Typecheck_Exp (Exp.Right.all);
         when WACC.AST.N_Assignment =>
            Typecheck_Exp (Exp.Assign_Left.all);
            Typecheck_Exp (Exp.Assign_Right.all);
         when WACC.AST.N_Conditional =>
            Typecheck_Exp (Exp.Condition.all);
            Typecheck_Exp (Exp.If_True.all);
            if Exp.If_False /= null then
               Typecheck_Exp (Exp.If_False.all);
            end if;
         when WACC.AST.N_Function_Call =>
            Symbol := WACC.Symbols.Get (Symbols, Exp.Function_Name);
            if Symbol.Typ = S_Int then
               raise Semantic_Error with "Variable used as function name";
            elsif Symbol.Typ = S_FunType and then
                  Symbol.Param_Count /= Natural (Length (Exp.Args))
            then
               raise Semantic_Error with "Function called with wrong number of arguments";
            end if;

            for Arg of Exp.Args loop
               Typecheck_Exp (Arg.all);
            end loop;
      end case;
   end Typecheck_Exp;

   procedure Typecheck_Optional_Exp
      (Tree : WACC.AST.Any_Exp_Node)
   is
      use type WACC.AST.Any_Exp_Node;
   begin
      if Tree /= null then
         Typecheck_Exp (Tree.all);
      end if;
   end Typecheck_Optional_Exp;

   procedure Typecheck_Variable_Declaration
      (Decl : in out WACC.AST.Variable_Declaration_Node)
   is
      use type WACC.AST.Any_Exp_Node;
      T : constant WACC.Symbols.Type_Node := (Typ => WACC.Symbols.S_Int);
   begin
      WACC.Symbols.Add (Symbols, Decl.Name, T);
      if Decl.Init /= null then
         Typecheck_Exp (Decl.Init.all);
      end if;
   end Typecheck_Variable_Declaration;

   procedure Typecheck_Function_Declaration
      (Decl : in out WACC.AST.Function_Declaration_Node)
   is
      use WACC.Symbols;
      use WACC.AST.Identifier_Vectors;
      use type WACC.AST.Any_Block_Node;

      Fun_Type : Type_Node :=
         (Typ         => S_FunType,
          Param_Count => Natural (Length (Decl.Params)),
          Defined     => False);
      Has_Body : constant Boolean := Decl.FBody /= null;
      Already_Defined : Boolean := False;
      Old_Decl : Any_Type_Node;
   begin
      if Contains (Symbols, Decl.Name) then
         Old_Decl := Get (Symbols, Decl.Name);
         if Old_Decl.Typ /= Fun_Type.Typ or else
            Old_Decl.Param_Count /= Fun_Type.Param_Count
         then
            raise Semantic_Error with "Incompatible function declarations";
         end if;
         Already_Defined := Old_Decl.Defined;
         if Already_Defined and then Has_Body then
            raise Semantic_Error with "Function is defined more than once";
         end if;
      end if;

      Fun_Type.Defined := Already_Defined or else Has_Body;
      Add (Symbols, Decl.Name, Fun_Type);

      if Has_Body then
         for Param of Decl.Params loop
            Add (Symbols, Param, Type_Node'(Typ => S_Int));
         end loop;
         Typecheck_Block (Decl.FBody.all);
      end if;
   end Typecheck_Function_Declaration;

   procedure Typecheck_Declaration
      (Tree : in out WACC.AST.Declaration_Node)
   is
   begin
      case Tree.Typ is
         when WACC.AST.N_VarDecl =>
            Typecheck_Variable_Declaration (Tree.Variable_Declaration.all);
         when WACC.AST.N_FunDecl =>
            Typecheck_Function_Declaration (Tree.Function_Declaration.all);
      end case;
   end Typecheck_Declaration;

   procedure Typecheck_Statement
      (Tree : in out WACC.AST.Statement_Node)
   is
      use type WACC.AST.Any_Statement_Node;
   begin
      case Tree.Typ is
         when WACC.AST.N_Return | WACC.AST.N_Expression =>
            Typecheck_Exp (Tree.Exp.all);
         when WACC.AST.N_If =>
            Typecheck_Exp (Tree.Condition.all);
            Typecheck_Statement (Tree.If_True.all);
            if Tree.If_False /= null then
               Typecheck_Statement (Tree.If_False.all);
            end if;
         when WACC.AST.N_Compound =>
            Typecheck_Block (Tree.Block.all);
         when WACC.AST.N_While | WACC.AST.N_DoWhile =>
            Typecheck_Exp (Tree.While_Condition.all);
            Typecheck_Statement (Tree.While_Body.all);
         when WACC.AST.N_For =>
            Typecheck_For_Init (Tree.For_Init.all);
            Typecheck_Optional_Exp (Tree.For_Condition);
            Typecheck_Optional_Exp (Tree.For_Post);
            Typecheck_Statement (Tree.For_Body.all);
         when WACC.AST.N_Break | WACC.AST.N_Continue | WACC.AST.N_Null =>
            null;
      end case;
   end Typecheck_Statement;

   procedure Typecheck_For_Init
      (Tree : in out WACC.AST.For_Init_Node)
   is
   begin
      case Tree.Typ is
         when WACC.AST.N_Init_Declaration =>
            Typecheck_Variable_Declaration (Tree.Decl.all);
         when WACC.AST.N_Init_Expression =>
            Typecheck_Optional_Exp (Tree.Exp);
      end case;
   end Typecheck_For_Init;

   procedure Typecheck_Block_Item
      (Tree : in out WACC.AST.Block_Item_Node)
   is
   begin
      case Tree.Typ is
         when WACC.AST.N_Statement =>
            Typecheck_Statement (Tree.Stmt.all);
         when WACC.AST.N_Declaration =>
            Typecheck_Declaration (Tree.Decl.all);
      end case;
   end Typecheck_Block_Item;

   procedure Typecheck_Block
      (Tree : in out WACC.AST.Block_Node)
   is
      use type WACC.AST.Any_Block_Item_Node;
      Node : WACC.AST.Any_Block_Item_Node := Tree.Head;
   begin
      while Node /= null loop
         Typecheck_Block_Item (Node.all);
         Node := Node.Next;
      end loop;
   end Typecheck_Block;

   procedure Analyze
      (Tree : in out WACC.AST.Program_Node)
   is
      use type WACC.AST.Any_Block_Node;
      Vars  : Identifier_Map := Identifier_Entry_Maps.Empty_Map;
      Label : Identifier := Null_Identifier;
   begin
      for Decl of Tree.Function_Declarations loop
         Resolve_Function_Declaration (Decl.all, Vars);
         Typecheck_Function_Declaration (Decl.all);
         if Decl.FBody /= null then
            Label_Block (Decl.FBody.all, Label);
         end if;
      end loop;
   end Analyze;

end WACC.Semantic_Analysis;
