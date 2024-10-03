private with Ada.Containers.Hashed_Maps;
with WACC.Strings;

package WACC.Symbols is
   type Type_Node;
   type Any_Type_Node is access Type_Node;

   type Type_Type is (S_Int, S_FunType);
   type Type_Node
      (Typ : Type_Type)
   is record
      case Typ is
         when S_Int =>
            null;
         when S_FunType =>
            Param_Count : Natural;
      end case;
   end record;

   type Table is private;

   procedure Add
      (This : in out Table;
       Name : WACC.Strings.Identifier;
       Node : Type_Node);
private
   package Type_Maps is new Ada.Containers.Hashed_Maps
      (Key_Type        => WACC.Strings.Identifier,
       Element_Type    => Any_Type_Node,
       Hash            => WACC.Strings.Hash,
       Equivalent_Keys => WACC.Strings."=");

   type Table is record
      Types : Type_Maps.Map;
   end record;
end WACC.Symbols;
