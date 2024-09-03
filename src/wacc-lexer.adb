pragma Style_Checks ("M120");
with Ada.Text_IO;
with WACC.IO;

package body WACC.Lexer is
   procedure Lex
      (Input_File : String;
       Tokens     : out Token_List)
   is
      File : IO.Reader;

      procedure Add_Single
         (Typ : Token_Type);
      procedure Lex_Identifier;
      procedure Lex_Integer;
      function Match
         (Str : String)
         return Boolean;

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
         T.Typ := T_Constant;
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
            exit when Ch not in 'a' .. 'z' | 'A' .. 'Z' | '_' | '0' .. '9';
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
         elsif T.Literal = "if" then
            T.Typ := T_if;
         elsif T.Literal = "else" then
            T.Typ := T_else;
         elsif T.Literal = "goto" then
            T.Typ := T_goto;
         elsif T.Literal = "do" then
            T.Typ := T_do;
         elsif T.Literal = "while" then
            T.Typ := T_while;
         elsif T.Literal = "for" then
            T.Typ := T_for;
         elsif T.Literal = "break" then
            T.Typ := T_break;
         elsif T.Literal = "continue" then
            T.Typ := T_continue;
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
         elsif Match ("&&") then
            Token_Vectors.Append (Tokens, (Typ => T_And_And, others => <>));
         elsif Match ("||") then
            Token_Vectors.Append (Tokens, (Typ => T_Pipe_Pipe, others => <>));
         elsif Match ("==") then
            Token_Vectors.Append (Tokens, (Typ => T_Equal_Equal, others => <>));
         elsif Match ("!=") then
            Token_Vectors.Append (Tokens, (Typ => T_Bang_Equal, others => <>));
         elsif Match ("<=") then
            Token_Vectors.Append (Tokens, (Typ => T_Less_Equal, others => <>));
         elsif Match (">=") then
            Token_Vectors.Append (Tokens, (Typ => T_Greater_Equal, others => <>));
         elsif Ch = '-' then
            Add_Single (T_Dash);
         elsif Ch = '+' then
            Add_Single (T_Plus);
         elsif Ch = '/' then
            Add_Single (T_Slash);
         elsif Ch = '*' then
            Add_Single (T_Asterisk);
         elsif Ch = '%' then
            Add_Single (T_Percent);
         elsif Ch = '~' then
            Add_Single (T_Tilde);
         elsif Ch = '!' then
            Add_Single (T_Bang);
         elsif Ch = '<' then
            Add_Single (T_Less_Than);
         elsif Ch = '>' then
            Add_Single (T_Greater_Than);
         elsif Ch = '=' then
            Add_Single (T_Equal);
         elsif Ch = '(' then
            Add_Single (T_Open_Paren);
         elsif Ch = ')' then
            Add_Single (T_Close_Paren);
         elsif Ch = '{' then
            Add_Single (T_Open_Brace);
         elsif Ch = '}' then
            Add_Single (T_Close_Brace);
         elsif Ch = '?' then
            Add_Single (T_Question);
         elsif Ch = ':' then
            Add_Single (T_Colon);
         elsif Ch = ';' then
            Add_Single (T_Semicolon);
         elsif Ch = ',' then
            Add_Single (T_Comma);
         else
            raise Lex_Error with "Unexpected character in input: " & Character'Pos (Ch)'Image & " '" & Ch & "'";
         end if;
      end loop;
      IO.Close (File);
   end Lex;

   function Image
      (T : Token)
      return String
   is
   begin
      if Length (T.Literal) > 0 then
         return T.Typ'Image & " """ & To_String (T.Literal) & """";
      else
         return T.Typ'Image;
      end if;
   end Image;

   procedure Print
      (Tokens : Token_List)
   is
      use Ada.Text_IO;
   begin
      Put_Line (Standard_Error, "[Lexer]");
      for Tok of Tokens loop
         Put_Line (Standard_Error, Image (Tok));
      end loop;
   end Print;

end WACC.Lexer;
