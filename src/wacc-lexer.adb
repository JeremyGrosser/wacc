with WACC.IO;

package body WACC.Lexer
   with SPARK_Mode => On
is

   procedure Lex
      (Input_File : String;
       Tokens     : out Token_List)
   is
      File : IO.File_Type;
   begin
      IO.Open (File, Input_File);
      while not IO.End_Of_File (File) loop
         case IO.Next (File) is
            when ' ' | ASCII.CR | ASCII.LF | ASCII.HT =>
               IO.Advance (File);
            when others =>
               IO.Advance (File);
         end case;
      end loop;
      IO.Close (File);
   end Lex;

end WACC.Lexer;
