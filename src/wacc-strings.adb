package body WACC.Strings is

   Identifier_Index : Natural := 0;

   function Make_Identifier
      (Prefix : String := "tmp.")
      return Identifier
   is
      use Ada.Strings.Unbounded;
      S : Unbounded_String := To_Unbounded_String (Prefix);
      N : constant String := Identifier_Index'Image;
   begin
      for Ch of N loop
         if Ch /= ' ' then
            Append (S, Ch);
         end if;
      end loop;
      Identifier_Index := Identifier_Index + 1;
      return S;
   end Make_Identifier;

end WACC.Strings;
