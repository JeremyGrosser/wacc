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

      function Parse_Exp
         return WACC.AST.Any_Exp_Node
      is
         Tok : constant WACC.Lexer.Token := First_Element (Input);
         Node : WACC.AST.Any_Exp_Node;
      begin
         if Tok.Typ = WACC.Lexer.T_int then
            Node := new WACC.AST.Exp_Node'(Typ => WACC.AST.N_Constant, Int => 0);
            for Ch of Ada.Strings.Unbounded.To_String (Tok.Literal) loop
               if Ch not in '0' .. '9' then
                  raise Program_Error with "Expected 0 .. 9 in int constant, got '" & Ch & "'";
               end if;
               Node.Int := Node.Int * 10 + Character'Pos (Ch) - Character'Pos ('0');
            end loop;
            Delete_First (Input);
            return Node;
         else
            raise Parse_Error with "Expected Constant in exp";
         end if;
      end Parse_Exp;

      function Parse_Statement
         return WACC.AST.Any_Statement_Node
      is
         Node : WACC.AST.Any_Statement_Node;
      begin
         Expect (WACC.Lexer.T_return);
         Node := new WACC.AST.Statement_Node'(Typ => WACC.AST.N_Return, Exp => null);
         Node.Exp := Parse_Exp;
         Expect (WACC.Lexer.T_Semicolon);
         return Node;
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
         Node.FBody := Parse_Statement;
         Expect (WACC.Lexer.T_Close_Brace);
      end Parse_Function;
   begin
      Parse_Function (Tree.Function_Definition);
      if not Is_Empty (Input) then
         raise Parse_Error with "Unexpected tokens after function definition";
      end if;
   end Parse_Program;

end WACC.Parser;
