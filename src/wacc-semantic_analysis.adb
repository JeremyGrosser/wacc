pragma Style_Checks ("M120");
with WACC.Strings; use WACC.Strings;
with Ada.Containers.Hashed_Maps;

package body WACC.Semantic_Analysis is

   package Variable_Maps is new Ada.Containers.Hashed_Maps
      (Key_Type         => Identifier,
       Element_Type     => Identifier,
       Hash             => WACC.Strings.Hash,
       Equivalent_Keys  => WACC.Strings."=");
   use Variable_Maps;
   Vars : Variable_Maps.Map := Variable_Maps.Empty_Map;

   procedure Resolve
      (Exp : in out WACC.AST.Exp_Node)
   is
      use type WACC.AST.Exp_Type;
   begin
      case Exp.Typ is
         when WACC.AST.N_Constant =>
            null;
         when WACC.AST.N_Var =>
            if Contains (Vars, Exp.Name) then
               Exp.Name := Element (Vars, Exp.Name);
            else
               raise Semantic_Error with "Undeclared variable in initializer: " & To_String (Exp.Name);
            end if;
         when WACC.AST.N_Unary =>
            Resolve (Exp.Exp.all);
         when WACC.AST.N_Binary =>
            Resolve (Exp.Left.all);
            Resolve (Exp.Right.all);
         when WACC.AST.N_Assignment =>
            if Exp.Assign_Left.Typ /= WACC.AST.N_Var then
               raise Semantic_Error with "Invalid lvalue!";
            else
               Resolve (Exp.Assign_Left.all);
               Resolve (Exp.Assign_Right.all);
            end if;
      end case;
   end Resolve;

   procedure Resolve
      (Decl : in out WACC.AST.Declaration_Node)
   is
   begin
      if Contains (Vars, Decl.Name) then
         raise Semantic_Error with "Variable " & To_String (Decl.Name) & " has already been declared!";
      end if;

      declare
         use type WACC.AST.Any_Exp_Node;

         Unique : constant Identifier := Make_Identifier
            (Prefix => To_String (Decl.Name) & ".");
      begin
         Include (Vars, Decl.Name, Unique);
         if Decl.Init /= null then
            Resolve (Decl.Init.all);
         end if;

         Decl.Name := Unique;
      end;
   end Resolve;

   procedure Resolve
      (Tree : in out WACC.AST.Statement_Node)
   is
   begin
      case Tree.Typ is
         when WACC.AST.N_Return | WACC.AST.N_Expression =>
            Resolve (Tree.Exp.all);
         when WACC.AST.N_Null =>
            null;
      end case;
   end Resolve;

   procedure Analyze
      (Tree : in out WACC.AST.Statement_Node)
   is
   begin
      Resolve (Tree);
   end Analyze;

   procedure Analyze
      (Tree : in out WACC.AST.Declaration_Node)
   is
   begin
      Resolve (Tree);
   end Analyze;

   procedure Analyze
      (Tree : in out WACC.AST.Block_Item_Node)
   is
   begin
      case Tree.Typ is
         when WACC.AST.N_Statement =>
            Analyze (Tree.Stmt.all);
         when WACC.AST.N_Declaration =>
            Analyze (Tree.Decl.all);
      end case;
   end Analyze;

   procedure Analyze
      (Tree : in out WACC.AST.Function_Definition_Node)
   is
      use type WACC.AST.Any_Block_Item_Node;
      Node : WACC.AST.Any_Block_Item_Node := Tree.FBody;
   begin
      while Node /= null loop
         Analyze (Node.all);
         Node := Node.Next;
      end loop;
   end Analyze;

   procedure Analyze
      (Tree : in out WACC.AST.Program_Node)
   is
   begin
      Analyze (Tree.Function_Definition);
   end Analyze;

end WACC.Semantic_Analysis;
