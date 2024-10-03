pragma Style_Checks ("M120");
package body WACC.Symbols is
   procedure Add
      (This : in out Table;
       Name : WACC.Strings.Identifier;
       Node : Type_Node)
   is
      TN : constant Any_Type_Node := new Type_Node (Typ => Node.Typ);
   begin
      TN.all := Node;
      if Type_Maps.Contains (This.Types, Name) then
         Type_Maps.Replace (This.Types, Name, TN);
      else
         Type_Maps.Insert (This.Types, Name, TN);
      end if;
   end Add;

   function Get
      (This : in out Table;
       Name : WACC.Strings.Identifier)
       return Any_Type_Node
   is
   begin
      if Contains (This, Name) then
         return Type_Maps.Element (This.Types, Name);
      else
         raise Symbol_Error with "Unknown symbol: " & WACC.Strings.To_String (Name);
      end if;
   end Get;

   function Contains
      (This : in out Table;
       Name : WACC.Strings.Identifier)
       return Boolean
   is (Type_Maps.Contains (This.Types, Name));
end WACC.Symbols;
