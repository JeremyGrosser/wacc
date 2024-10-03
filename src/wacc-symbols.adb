package body WACC.Symbols is
   procedure Add
      (This : in out Table;
       Name : WACC.Strings.Identifier;
       Node : Type_Node)
   is
      TN : constant Any_Type_Node := new Type_Node (Typ => Node.Typ);
   begin
      TN.all := Node;
      Type_Maps.Insert (This.Types, Name, TN);
   end Add;
end WACC.Symbols;
