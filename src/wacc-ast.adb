with Ada.Text_IO;

package body WACC.AST is

   procedure Print
      (This : Program_Node)
   is
      Indent_Level : Natural := 0;

      procedure Log
         (Str : String)
      is
         use Ada.Text_IO;
      begin
         for I in 1 .. Indent_Level loop
            Put (Standard_Error, ' ');
         end loop;
         Put (Standard_Error, Str);
         New_Line (Standard_Error);
      end Log;

      procedure Indent is
      begin
         Indent_Level := Indent_Level + 1;
      end Indent;

      procedure Dedent is
      begin
         Indent_Level := Indent_Level - 1;
      end Dedent;

      procedure Print
         (This : Identifier)
      is
      begin
         Log (To_String (This));
      end Print;

      procedure Print
         (This : Exp_Node);

      procedure Print
         (This : Binary_Operator_Node)
      is
      begin
         Log ("Binary_Operator");
         Indent;
         Log (This.Typ'Image);
         Dedent;
      end Print;

      procedure Print
         (This : Unary_Operator_Node)
      is
      begin
         Log ("Unary_Operator");
         Indent;
         Log (This.Typ'Image);
         Dedent;
      end Print;

      procedure Print
         (This : Exp_Node)
      is
      begin
         Log ("Exp");
         Indent;
         Log (This.Typ'Image);
         case This.Typ is
            when N_Constant =>
               Log (This.Int'Image);
            when N_Var =>
               Print (This.Name);
            when N_Unary =>
               Print (This.Unary_Operator.all);
               Print (This.Exp.all);
            when N_Binary =>
               Print (This.Binary_Operator.all);
               Log ("Left");
               Indent;
               Print (This.Left.all);
               Dedent;
               Log ("Right");
               Indent;
               Print (This.Right.all);
               Dedent;
            when N_Assignment =>
               Print (This.Assign_Left.all);
               Log (" = ");
               Print (This.Assign_Right.all);
            when N_Conditional =>
               Print (This.Condition.all);
               Log (" ? ");
               Print (This.If_True.all);
               Log (" : ");
               Print (This.If_False.all);
         end case;
         Dedent;
      end Print;

      procedure Print
         (This : Declaration_Node);

      procedure Print
         (This : For_Init_Node)
      is
      begin
         Log ("For_Init");
         Indent;
         Log (This.Typ'Image);
         Indent;
         case This.Typ is
            when N_Init_Declaration =>
               Print (This.Decl.all);
            when N_Init_Expression =>
               if This.Exp /= null then
                  Print (This.Exp.all);
               else
                  Log ("(null)");
               end if;
         end case;
         Dedent;
      end Print;

      procedure Print
         (This : Block_Node);

      procedure Print
         (This : Statement_Node);

      procedure Print
         (This : Statement_Node)
      is
      begin
         Log ("Statement");
         Indent;
         Log (This.Typ'Image);
         case This.Typ is
            when N_Return | N_Expression =>
               if This.Exp /= null then
                  Print (This.Exp.all);
               else
                  Log ("(null)");
               end if;
            when N_If =>
               Log ("Condition");
               Indent;
               Print (This.Condition.all);
               Dedent;
               Log ("If_True");
               Indent;
               Print (This.If_True.all);
               Dedent;
               if This.If_False /= null then
                  Log ("If_False");
                  Indent;
                  Print (This.If_False.all);
                  Dedent;
               end if;
            when N_Compound =>
               Print (This.Block.all);
            when N_While | N_DoWhile =>
               Log ("Condition");
               Indent;
               Print (This.While_Condition.all);
               Dedent;
               Log ("Body");
               Indent;
               Print (This.While_Body.all);
               Dedent;
            when N_For =>
               Log ("Init");
               Indent;
               Print (This.For_Init.all);
               Dedent;
               if This.For_Condition /= null then
                  Log ("Condition");
                  Indent;
                  Print (This.For_Condition.all);
                  Dedent;
               end if;

               if This.For_Post /= null then
                  Log ("Post");
                  Indent;
                  Print (This.For_Post.all);
                  Dedent;
               end if;

               Log ("Body");
               Indent;
               Print (This.For_Body.all);
               Dedent;
            when N_Break | N_Continue | N_Goto | N_Label =>
               Print (This.Label);
            when N_Null =>
               null;
         end case;
         Dedent;
      end Print;

      procedure Print
         (This : Declaration_Node)
      is
      begin
         Log ("Declaration");
         Indent;
         Print (This.Name);
         if This.Init /= null then
            Print (This.Init.all);
         else
            Log ("(null)");
         end if;
         Dedent;
      end Print;

      procedure Print
         (This : Block_Item_Node)
      is
      begin
         Log ("Block_Item");
         Indent;
         Log (This.Typ'Image);
         case This.Typ is
            when N_Statement =>
               Print (This.Stmt.all);
            when N_Declaration =>
               Print (This.Decl.all);
         end case;
         Dedent;
         if This.Next /= null then
            Print (This.Next.all);
         else
            Log ("(End of Block_Item list)");
         end if;
      end Print;

      procedure Print
         (This : Block_Node)
      is
      begin
         Log ("Block");
         Indent;
         if This.Head /= null then
            Print (This.Head.all);
         else
            Log ("(null)");
         end if;
         Dedent;
      end Print;

      procedure Print
         (This : Function_Definition_Node)
      is
      begin
         Log ("Function_Definition");
         Indent;
         Print (This.Name);
         if This.FBody /= null then
            Print (This.FBody.all);
         else
            Log ("(null)");
         end if;
         Dedent;
      end Print;
   begin
      Log ("[AST]");
      Indent;
      Log ("Program");
      Indent;
      Print (This.Function_Definition);
      Dedent;
      Dedent;
   end Print;

end WACC.AST;
