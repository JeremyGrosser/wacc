with Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Containers;

package WACC.Strings is

   subtype Identifier is Ada.Strings.Unbounded.Unbounded_String;

   Null_Identifier : constant Identifier :=
      Ada.Strings.Unbounded.Null_Unbounded_String;

   function Make_Identifier
      (Prefix : String := "tmp.")
      return Identifier;

   function To_String
      (Id : Identifier)
      return String
   renames Ada.Strings.Unbounded.To_String;

   function Hash
      (Id : Identifier)
      return Ada.Containers.Hash_Type
   renames Ada.Strings.Unbounded.Hash;

   function "="
      (Left, Right : Identifier)
      return Boolean
   renames Ada.Strings.Unbounded."=";

end WACC.Strings;
