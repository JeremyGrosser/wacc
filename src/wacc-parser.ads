with WACC.Lexer;
with WACC.AST;

package WACC.Parser
   with Elaborate_Body
is
   Parse_Error : exception;

   procedure Parse_Program
      (Tokens : WACC.Lexer.Token_List;
       Tree   : out WACC.AST.Program_Node);

end WACC.Parser;
