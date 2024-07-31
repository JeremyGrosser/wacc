pragma Style_Checks ("M120");
with Ada.Strings.Unbounded;

package body WACC.Parser is

   procedure Parse_Program
      (Tokens : WACC.Lexer.Token_List;
       Tree   : out WACC.AST.Program_Node)
   is
      use type WACC.Lexer.Token_Type;
      use WACC.Lexer.Token_Vectors;
      Input : WACC.Lexer.Token_List := Tokens;

      procedure Expect
         (Typ : WACC.Lexer.Token_Type)
      is
      begin
         if First_Element (Input).Typ = Typ then
            Delete_First (Input);
         else
            raise Parse_Error with "Expected token" & Typ'Image & ", found" &
                                   First_Element (Input).Typ'Image & " instead";
         end if;
      end Expect;

      procedure Parse_Unop
         (Node : out WACC.AST.Any_Unary_Operator_Node)
      is
         Tok : constant WACC.Lexer.Token := First_Element (Input);
      begin
         case Tok.Typ is
            when WACC.Lexer.T_Dash =>
               Node := new WACC.AST.Unary_Operator_Node'(Typ => WACC.AST.N_Negate);
            when WACC.Lexer.T_Tilde =>
               Node := new WACC.AST.Unary_Operator_Node'(Typ => WACC.AST.N_Complement);
            when others =>
               raise Parse_Error with "Expected Dash or Tilde token in unary operator, got " & Tok.Typ'Image;
         end case;
         Delete_First (Input);
      end Parse_Unop;

      function Parse_Int
         (Str : String)
         return Long_Integer
      is
         N : Long_Integer := 0;
      begin
         for Ch of Str loop
            if Ch not in '0' .. '9' then
               raise Parse_Error with "Expected 0 .. 9 in int constant, got '" & Ch & "'";
            end if;
            N := N * 10 + Long_Integer (Character'Pos (Ch) - Character'Pos ('0'));
         end loop;
         return N;
      end Parse_Int;

      procedure Parse_Exp
         (Node : out WACC.AST.Any_Exp_Node)
      is
         Tok : constant WACC.Lexer.Token := First_Element (Input);
      begin
         case Tok.Typ is
            when WACC.Lexer.T_int =>
               Node := new WACC.AST.Exp_Node'
                  (Typ => WACC.AST.N_Constant,
                   Int => Parse_Int (Ada.Strings.Unbounded.To_String (Tok.Literal)));
               Delete_First (Input);
            when WACC.Lexer.T_Dash | WACC.Lexer.T_Tilde =>
               Node := new WACC.AST.Exp_Node'(Typ => WACC.AST.N_Unary, others => <>);
               Parse_Unop (Node.Unary_Operator);
               Parse_Exp (Node.Exp);
            when WACC.Lexer.T_Open_Paren =>
               Delete_First (Input);
               Parse_Exp (Node);
               Expect (WACC.Lexer.T_Close_Paren);
            when others =>
               raise Parse_Error with "Unexpected token in exp: " & Tok.Typ'Image;
         end case;
      end Parse_Exp;

      procedure Parse_Statement
         (Node : out WACC.AST.Any_Statement_Node)
      is
      begin
         Expect (WACC.Lexer.T_return);
         Node := new WACC.AST.Statement_Node'(Typ => WACC.AST.N_Return, Exp => null);
         Parse_Exp (Node.Exp);
         Expect (WACC.Lexer.T_Semicolon);
      end Parse_Statement;

      procedure Parse_Function
         (Node : in out WACC.AST.Function_Definition_Node)
      is
      begin
         Expect (WACC.Lexer.T_int);
         if First_Element (Input).Typ = WACC.Lexer.T_Identifier then
            Node.Name := First_Element (Input).Literal;
            Delete_First (Input);
         else
            raise Parse_Error with "Expected identifier after ""int""";
         end if;
         Expect (WACC.Lexer.T_Open_Paren);
         Expect (WACC.Lexer.T_void);
         Expect (WACC.Lexer.T_Close_Paren);
         Expect (WACC.Lexer.T_Open_Brace);
         Parse_Statement (Node.FBody);
         Expect (WACC.Lexer.T_Close_Brace);
      end Parse_Function;
   begin
      Parse_Function (Tree.Function_Definition);
      if not Is_Empty (Input) then
         raise Parse_Error with "Unexpected tokens after function definition";
      end if;
   end Parse_Program;

end WACC.Parser;
