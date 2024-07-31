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
            when N_Unary =>
               Print (This.Unary_Operator.all);
            when N_Binary =>
               Print (This.Left.all);
               Print (This.Binary_Operator.all);
               Print (This.Right.all);
         end case;
         Dedent;
      end Print;

      procedure Print
         (This : Statement_Node)
      is
      begin
         Log ("Statement");
         Indent;
         Log (This.Typ'Image);
         case This.Typ is
            when N_Return =>
               if This.Exp /= null then
                  Print (This.Exp.all);
               else
                  Log ("(null)");
               end if;
         end case;
         Dedent;
      end Print;

      procedure Print
         (This : Function_Definition_Node)
      is
      begin
         Log ("Function_Definition");
         Indent;
         Log ("Name = " & To_String (This.Name));
         Log ("FBody = ");
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
