with WACC.Lexer;
with WACC.AST;

package WACC.Parser
   with Elaborate_Body
is

   procedure Parse
      (Tokens : WACC.Lexer.Token_List;
       Tree   : out WACC.AST.Program_Node);

end WACC.Parser;
