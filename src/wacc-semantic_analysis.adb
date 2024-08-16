pragma Style_Checks ("M120");
with WACC.Strings; use WACC.Strings;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;

package body WACC.Semantic_Analysis is
   --  There are two sets of procedures here, Resolve_Variables and Analyze.
   --  Analyze is the top level tree walk. When it descends into a Statement or
   --  Declaration it calls Resolve_Variables. Resolve_Variables procedures do variable renaming.
   --  Analyze will be extended in the future to call more analysis passes.

   package Identifier_Maps is new Ada.Containers.Hashed_Maps
      (Key_Type         => Identifier,
       Element_Type     => Identifier,
       Hash             => WACC.Strings.Hash,
       Equivalent_Keys  => WACC.Strings."=");
   use Identifier_Maps;
   Vars : Identifier_Maps.Map := Identifier_Maps.Empty_Map;

   package Identifier_Sets is new Ada.Containers.Hashed_Sets
      (Element_Type        => Identifier,
       Hash                => WACC.Strings.Hash,
       Equivalent_Elements => WACC.Strings."=");
   use Identifier_Sets;
   Labels : Identifier_Sets.Set := Identifier_Sets.Empty_Set;

   procedure Resolve_Variables
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
            Resolve_Variables (Exp.Exp.all);
         when WACC.AST.N_Binary =>
            Resolve_Variables (Exp.Left.all);
            Resolve_Variables (Exp.Right.all);
         when WACC.AST.N_Assignment =>
            if Exp.Assign_Left.Typ /= WACC.AST.N_Var then
               raise Semantic_Error with "Invalid lvalue!";
            else
               Resolve_Variables (Exp.Assign_Left.all);
               Resolve_Variables (Exp.Assign_Right.all);
            end if;
         when WACC.AST.N_Conditional =>
            Resolve_Variables (Exp.Condition.all);
            Resolve_Variables (Exp.If_True.all);
            Resolve_Variables (Exp.If_False.all);
      end case;
   end Resolve_Variables;

   procedure Resolve_Variables
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
            Resolve_Variables (Decl.Init.all);
         end if;

         Decl.Name := Unique;
      end;
   end Resolve_Variables;

   procedure Resolve_Variables
      (Tree : in out WACC.AST.Statement_Node)
   is
      use type WACC.AST.Any_Statement_Node;
   begin
      case Tree.Typ is
         when WACC.AST.N_Return | WACC.AST.N_Expression =>
            Resolve_Variables (Tree.Exp.all);
         when WACC.AST.N_If =>
            Resolve_Variables (Tree.Condition.all);
            Resolve_Variables (Tree.If_True.all);
            if Tree.If_False /= null then
               Resolve_Variables (Tree.If_False.all);
            end if;
         when WACC.AST.N_Goto | WACC.AST.N_Label | WACC.AST.N_Null =>
            null;
      end case;
   end Resolve_Variables;

   procedure Resolve_Labels
      (Tree : in out WACC.AST.Statement_Node;
       Next : WACC.AST.Any_Block_Item_Node)
   is
      use type WACC.AST.Any_Block_Item_Node;
      use type WACC.AST.Block_Item_Type;
   begin
      case Tree.Typ is
         when WACC.AST.N_Label =>
            if Contains (Labels, Tree.Label) then
               raise Semantic_Error with "Label names must be unique within a function";
            else
               Include (Labels, Tree.Label);
            end if;

            if Next = null then
               raise Semantic_Error with "Label without statement";
            elsif Next.Typ = WACC.AST.N_Declaration then
               raise Semantic_Error with "Label must be followed by a statement, not a declaration";
            end if;
         when WACC.AST.N_Goto =>
            null;
         when others =>
            null;
      end case;
   end Resolve_Labels;

   procedure Analyze
      (Tree : in out WACC.AST.Statement_Node;
       Next : WACC.AST.Any_Block_Item_Node)
   is
   begin
      Resolve_Variables (Tree);
      Resolve_Labels (Tree, Next);
   end Analyze;

   procedure Analyze
      (Tree : in out WACC.AST.Declaration_Node)
   is
   begin
      Resolve_Variables (Tree);
   end Analyze;

   procedure Analyze
      (Tree : in out WACC.AST.Block_Item_Node)
   is
   begin
      case Tree.Typ is
         when WACC.AST.N_Statement =>
            Analyze (Tree.Stmt.all, Next => Tree.Next);
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
