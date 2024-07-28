with WACC.AST;
with WACC.Assembly;

package WACC.Codegen is

   procedure Generate
      (Tree : WACC.AST.Program_Node;
       Asm  : out WACC.Assembly.Program_Node);

end WACC.Codegen;
