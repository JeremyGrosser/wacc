with WACC.AST;

package WACC.Semantic_Analysis is

   Semantic_Error : exception;

   procedure Analyze
      (Tree : in out WACC.AST.Program_Node);

end WACC.Semantic_Analysis;
