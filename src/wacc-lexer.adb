pragma Style_Checks ("M120");
with WACC.IO;

package body WACC.Lexer
   with SPARK_Mode => On
is
   procedure Lex
      (Input_File : String;
       Tokens     : out Token_List)
   is
      File : IO.Reader;

      function Match
         (Prefix : String)
         return Boolean
      is
      begin
         if IO.Lookahead (File, Prefix'Length) = Prefix then
            IO.Advance (File, Prefix'Length);
            return True;
         else
            return False;
         end if;
      end Match;

      procedure Add_Token
         (Typ : Token_Type)
      is
         T : constant Token := (Typ => Typ, others => <>);
      begin
         Token_Vectors.Append (Tokens, T);
      end Add_Token;

      procedure Lex_Integer is
         Ch : Character;
         T : Token;
      begin
         T.Typ := T_int;
         loop
            Ch := IO.Next (File);
            if Ch not in '0' .. '9' and then Ch in 'a' .. 'z' | 'A' .. 'Z' | '_' then
               raise Lex_Error with "Expected break after integer";
            end if;
            exit when Ch not in '0' .. '9';
            Append (T.Literal, Ch);
            exit when IO.End_Of_File (File);
            IO.Advance (File);
         end loop;
         Token_Vectors.Append (Tokens, T);
      end Lex_Integer;

      procedure Lex_Identifier is
         T : Token;
         Ch : Character;
      begin
         T.Typ := T_Identifier;
         loop
            Ch := IO.Next (File);
            exit when Ch not in 'a' .. 'z' | 'A' .. 'Z' | '_';
            Append (T.Literal, Ch);
            exit when IO.End_Of_File (File);
            IO.Advance (File);
         end loop;
         Token_Vectors.Append (Tokens, T);
      end Lex_Identifier;

      Ch : Character;
   begin
      IO.Open (File, Input_File);
      while not IO.End_Of_File (File) loop
         Ch := IO.Next (File);
         if Ch in ' ' | ASCII.CR | ASCII.LF | ASCII.HT then
            IO.Advance (File);
         elsif Match ("return") then
            Add_Token (T_return);
         elsif Match ("void") then
            Add_Token (T_void);
         elsif Ch in '0' .. '9' then
            Lex_Integer;
         elsif Ch in 'a' .. 'z' | 'A' .. 'Z' | '_' then
            Lex_Identifier;
         elsif Ch = '(' then
            IO.Advance (File);
            Add_Token (T_Open_Paren);
         elsif Ch = ')' then
            IO.Advance (File);
            Add_Token (T_Close_Paren);
         elsif Ch = '{' then
            IO.Advance (File);
            Add_Token (T_Open_Brace);
         elsif Ch = '}' then
            IO.Advance (File);
            Add_Token (T_Close_Brace);
         elsif Ch = ';' then
            IO.Advance (File);
            Add_Token (T_Semicolon);
         else
            raise Lex_Error with "Unexpected character in input: " & Character'Pos (Ch)'Image & " '" & Ch & "'";
         end if;
      end loop;
      IO.Close (File);
   end Lex;

end WACC.Lexer;
