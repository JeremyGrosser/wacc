with Ada.Text_IO;

package body WACC.AST is

   Indent_Level : Natural := 0;

   procedure Log
      (Str : String)
   is
   begin
      for I in 1 .. Indent_Level loop
         Ada.Text_IO.Put (' ');
      end loop;
      Ada.Text_IO.Put (Ada.Text_IO.Standard_Error, Str);
      Ada.Text_IO.New_Line;
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
      (This : Exp_Node)
   is
   begin
      Log ("Exp");
      Indent;
      Log (This.Typ'Image);
      case This.Typ is
         when T_Constant =>
            Log (This.Int'Image);
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
         when T_Return =>
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

   procedure Print
      (This : Program_Node)
   is
   begin
      Log ("Program");
      Indent;
      Print (This.Function_Definition);
      Dedent;
   end Print;

end WACC.AST;
