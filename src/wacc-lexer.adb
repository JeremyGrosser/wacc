pragma Style_Checks ("M120");
with WACC.IO;

package body WACC.Lexer is
   procedure Lex
      (Input_File : String;
       Tokens     : out Token_List)
   is
      File : IO.Reader;

      procedure Add_Single
         (Typ : Token_Type)
      is
         T : constant Token := (Typ => Typ, others => <>);
      begin
         --  Append (T.Literal, IO.Next (File));
         IO.Advance (File);
         Token_Vectors.Append (Tokens, T);
      end Add_Single;

      procedure Lex_Integer is
         Ch : Character;
         T : Token;
      begin
         T.Typ := T_int;
         loop
            Ch := IO.Next (File);
            if Ch not in '0' .. '9' then
               if Ch in 'a' .. 'z' | 'A' .. 'Z' | '_' then
                  raise Lex_Error with "Expected break after integer";
               else
                  exit;
               end if;
            end if;
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

         if T.Literal = "void" then
            T.Typ := T_void;
         elsif T.Literal = "int" then
            T.Typ := T_int;
         elsif T.Literal = "return" then
            T.Typ := T_return;
         end if;

         Token_Vectors.Append (Tokens, T);
      end Lex_Identifier;

      function Match
         (Str : String)
         return Boolean
      is
      begin
         for I in Str'Range loop
            if IO.Next (File) /= Str (I) then
               IO.Advance (File, -(I - Str'First));
               return False;
            end if;
            IO.Advance (File);
         end loop;
         return True;
      end Match;

      Ch : Character;
   begin
      IO.Open (File, Input_File);
      while not IO.End_Of_File (File) loop
         Ch := IO.Next (File);
         if Ch in ' ' | ASCII.CR | ASCII.LF | ASCII.HT then
            IO.Advance (File);
         elsif Ch in '0' .. '9' then
            Lex_Integer;
         elsif Ch in 'a' .. 'z' | 'A' .. 'Z' | '_' then
            Lex_Identifier;
         elsif Match ("--") then
            Token_Vectors.Append (Tokens, (Typ => T_Dash_Dash, others => <>));
         elsif Ch = '-' then
            Add_Single (T_Dash);
         elsif Ch = '~' then
            Add_Single (T_Tilde);
         elsif Ch = '(' then
            Add_Single (T_Open_Paren);
         elsif Ch = ')' then
            Add_Single (T_Close_Paren);
         elsif Ch = '{' then
            Add_Single (T_Open_Brace);
         elsif Ch = '}' then
            Add_Single (T_Close_Brace);
         elsif Ch = ';' then
            Add_Single (T_Semicolon);
         else
            raise Lex_Error with "Unexpected character in input: " & Character'Pos (Ch)'Image & " '" & Ch & "'";
         end if;
      end loop;
      IO.Close (File);
   end Lex;

end WACC.Lexer;
