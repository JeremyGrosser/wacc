pragma Style_Checks ("M120");
with WACC.Strings; use WACC.Strings;
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Hashed_Sets;

package body WACC.Semantic_Analysis is

   type Map_Entry is record
      New_Name             : Identifier;
      From_Current_Block   : Boolean;
   end record;

   package Identifier_Entry_Maps is new Ada.Containers.Hashed_Maps
      (Key_Type         => Identifier,
       Element_Type     => Map_Entry,
       Hash             => WACC.Strings.Hash,
       Equivalent_Keys  => WACC.Strings."=");
   use Identifier_Entry_Maps;

   subtype Variable_Map is Identifier_Entry_Maps.Map;

   --  Copy a map, setting From_Current_Block := False; for all elements.
   procedure Copy_Variable_Map
      (From : Variable_Map;
       To   : out Variable_Map)
   is
   begin
      for Cursor in Iterate (From) loop
         declare
            Item : Map_Entry := Element (Cursor);
         begin
            Item.From_Current_Block := False;
            Include (To, Key (Cursor), Item);
         end;
      end loop;
   end Copy_Variable_Map;

   package Identifier_Sets is new Ada.Containers.Hashed_Sets
      (Element_Type        => Identifier,
       Hash                => WACC.Strings.Hash,
       Equivalent_Elements => WACC.Strings."=");
   use Identifier_Sets;
   Labels : Identifier_Sets.Set;

   procedure Resolve_Block
      (Tree : in out WACC.AST.Block_Node;
       Vars : in out Variable_Map);

   procedure Resolve_Expression
      (Exp  : in out WACC.AST.Exp_Node;
       Vars : Variable_Map)
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
      end case;
   end Resolve_Expression;

   procedure Resolve_Declaration
      (Decl : in out WACC.AST.Declaration_Node;
       Vars : in out Variable_Map)
   is
   begin
      if Contains (Vars, Decl.Name) and then Element (Vars, Decl.Name).From_Current_Block then
         raise Semantic_Error with "Duplicate variable """ & To_String (Decl.Name) & """ has already been declared!";
      end if;

      declare
         use type WACC.AST.Any_Exp_Node;

         Unique : constant Identifier := Make_Identifier
            (Prefix => To_String (Decl.Name) & ".");
      begin
         Include (Vars, Decl.Name, Map_Entry'
            (New_Name           => Unique,
             From_Current_Block => True));
         if Decl.Init /= null then
            Resolve_Expression (Decl.Init.all, Vars);
         end if;

         Decl.Name := Unique;
      end;
   end Resolve_Declaration;

   procedure Resolve_Optional_Exp
      (Tree : WACC.AST.Any_Exp_Node;
       Vars : Variable_Map)
   is
      use type WACC.AST.Any_Exp_Node;
   begin
      if Tree /= null then
         Resolve_Expression (Tree.all, Vars);
      end if;
   end Resolve_Optional_Exp;

   procedure Resolve_For_Init
      (Tree : in out WACC.AST.For_Init_Node;
       Vars : in out Variable_Map)
   is
   begin
      case Tree.Typ is
         when WACC.AST.N_Init_Expression =>
            Resolve_Optional_Exp (Tree.Exp, Vars);
         when WACC.AST.N_Init_Declaration =>
            Resolve_Declaration (Tree.Decl.all, Vars);
      end case;
   end Resolve_For_Init;

   procedure Resolve_Statement
      (Tree : in out WACC.AST.Statement_Node;
       Vars : Variable_Map)
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
               New_Variable_Map : Variable_Map := Empty_Map;
            begin
               Copy_Variable_Map (Vars, New_Variable_Map);
               Resolve_Block (Tree.Block.all, New_Variable_Map);
            end;
         when WACC.AST.N_For =>
            declare
               New_Variable_Map : Variable_Map := Empty_Map;
            begin
               Copy_Variable_Map (Vars, New_Variable_Map);
               Resolve_For_Init (Tree.For_Init.all, New_Variable_Map);
               Resolve_Optional_Exp (Tree.For_Condition, New_Variable_Map);
               Resolve_Optional_Exp (Tree.For_Post, New_Variable_Map);
               Resolve_Statement (Tree.For_Body.all, New_Variable_Map);
            end;
         when WACC.AST.N_While | WACC.AST.N_DoWhile =>
            Resolve_Expression (Tree.While_Condition.all, Vars);
            Resolve_Statement (Tree.While_Body.all, Vars);
         when WACC.AST.N_Goto | WACC.AST.N_Label | WACC.AST.N_Null | WACC.AST.N_Break
            | WACC.AST.N_Continue =>
            null;
      end case;
   end Resolve_Statement;

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

            declare
               use type WACC.AST.Any_Exp_Node;
               N : WACC.AST.Any_Block_Item_Node := Next;
            begin
               while N /= null and then N.Typ = WACC.AST.N_Declaration loop
                  if N.Decl.Init /= null then
                     raise Semantic_Error with "Label cannot precede a declaration with initializer.";
                  end if;
                  N := N.Next;
               end loop;

               if N = null then
                  raise Semantic_Error with "Label must precede a statement";
               end if;
            end;
         when WACC.AST.N_Goto =>
            null;
         when others =>
            null;
      end case;
   end Resolve_Labels;

   procedure Resolve_Block_Item
      (Tree : in out WACC.AST.Block_Item_Node;
       Vars : in out Variable_Map)
   is
   begin
      case Tree.Typ is
         when WACC.AST.N_Statement =>
            Resolve_Statement (Tree.Stmt.all, Vars);
            Resolve_Labels (Tree.Stmt.all, Tree.Next);
         when WACC.AST.N_Declaration =>
            Resolve_Declaration (Tree.Decl.all, Vars);
      end case;
   end Resolve_Block_Item;

   procedure Resolve_Block
      (Tree : in out WACC.AST.Block_Node;
       Vars : in out Variable_Map)
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
         when WACC.AST.N_Goto | WACC.AST.N_Label =>
            --  Should Resolve_Labels be moved here?
            null;
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

   procedure Analyze
      (Tree : in out WACC.AST.Function_Definition_Node)
   is
      Vars : Variable_Map := Identifier_Entry_Maps.Empty_Map;
      Label : Identifier := Null_Identifier;
   begin
      Clear (Labels);
      Resolve_Block (Tree.FBody.all, Vars);
      Label_Block (Tree.FBody.all, Label);
   end Analyze;

   procedure Analyze
      (Tree : in out WACC.AST.Program_Node)
   is
   begin
      Analyze (Tree.Function_Definition);
   end Analyze;

end WACC.Semantic_Analysis;
