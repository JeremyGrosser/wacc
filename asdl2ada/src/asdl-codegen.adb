pragma Style_Checks ("M120");
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Containers.Vectors;
with Ada.Containers;
with Ada.Text_IO;

package body ASDL.Codegen is
   Level : Natural := 0;

   procedure Indent is
   begin
      Level := Level + 1;
   end Indent;

   procedure Dedent is
   begin
      Level := Level - 1;
   end Dedent;

   procedure New_Line is
   begin
      Ada.Text_IO.New_Line;
      for I in 1 .. Level loop
         Ada.Text_IO.Put ("   ");
      end loop;
   end New_Line;

   procedure Put
      (Str : String)
   is
   begin
      Ada.Text_IO.Put (Str);
   end Put;

   procedure Put
      (Str : Unbounded_String)
   is
   begin
      Put (To_String (Str));
   end Put;

   --   Name : Type_Name;
   type Ada_Definition_Type is record
      Name        : Unbounded_String;
      Type_Name   : Unbounded_String;
      Is_Access   : Boolean := False;
   end record;

   package Ada_Definition_Vectors is new Ada.Containers.Vectors
      (Positive, Ada_Definition_Type);

   type Ada_Discriminant_Type is record
      Name        : Unbounded_String;
      Definitions : Ada_Definition_Vectors.Vector;
   end record;

   package Ada_Discriminant_Vectors is new Ada.Containers.Vectors
      (Positive, Ada_Discriminant_Type);

   type Ada_Record_Type is record
      Name           : Unbounded_String;
      Discriminants  : Ada_Discriminant_Vectors.Vector;
      Definitions    : Ada_Definition_Vectors.Vector;
   end record;

   package Ada_Record_Vectors is new Ada.Containers.Vectors
      (Positive, Ada_Record_Type);

   package Ada_Enum_Value_Vectors is new Ada.Containers.Vectors
      (Positive, Unbounded_String);

   type Ada_Enum_Type is record
      Name   : Unbounded_String;
      Values : Ada_Enum_Value_Vectors.Vector;
   end record;

   package Ada_Enum_Vectors is new Ada.Containers.Vectors
      (Positive, Ada_Enum_Type);

   type Ada_Package_Type is record
      Name     : Unbounded_String;
      Records  : Ada_Record_Vectors.Vector;
      Enums    : Ada_Enum_Vectors.Vector;
   end record;

   function To_String
      (N : Natural)
      return String
   is
      Digit : constant array (0 .. 9) of Character := "0123456789";
      S : String (1 .. 4); --  max digits
      I : Natural := S'Last;
      X : Natural := N;
   begin
      loop
         S (I) := Digit (X mod 10);
         I := I - 1;
         X := X / 10;
         exit when X = 0 or else I < S'First;
      end loop;
      return S (I + 1 .. S'Last);
   end To_String;

   function To_Upper
      (Ch : Character)
      return Character
   is
   begin
      if Ch in 'a' .. 'z' then
         return Character'Val
            (Character'Pos (Ch) - Character'Pos ('a') + Character'Pos ('A'));
      else
         return Ch;
      end if;
   end To_Upper;

   function To_Ada_Case
      (S : String)
      return String
   is
      X : String := S;
      I : Natural := X'First;
   begin
      loop
         if I = X'First then
            X (I) := To_Upper (X (I));
         elsif I < X'Last and then X (I) = '_' then
            X (I + 1) := To_Upper (X (I + 1));
         end if;
         I := I + 1;
         exit when I > X'Last;
      end loop;
      return X;
   end To_Ada_Case;

   function To_Ada_Case
      (S : Unbounded_String)
      return Unbounded_String
   is (To_Unbounded_String (To_Ada_Case (To_String (S))));

   procedure Generate_Definitions
      (Definitions : out Ada_Definition_Vectors.Vector;
       Fields      : ASDL.Parser.ASDL_Field_Vectors.Vector)
   is
      package String_Maps is new Ada.Containers.Indefinite_Hashed_Maps
         (Key_Type         => String,
          Element_Type     => Natural,
          Hash             => Ada.Strings.Hash,
          Equivalent_Keys  => "=");
      Count : String_Maps.Map := String_Maps.Empty_Map;

      function Field_Name
         (F : ASDL.Parser.ASDL_Field)
         return Unbounded_String
      is
         use String_Maps;
         Name : constant String := To_String (F.Name);
         N : Natural;
      begin
         if F.Id = Null_Unbounded_String then
            if not Contains (Count, Name) then
               Insert (Count, Name, 0);
            end if;

            N := Element (Count, Name);
            Delete (Count, Name);
            N := N + 1;
            Insert (Count, Name, N);

            return To_Unbounded_String (Name & "_" & To_String (Element (Count, Name)));
         else
            return F.Id;
         end if;
      end Field_Name;

      function Field_Type
         (F : ASDL.Parser.ASDL_Field)
         return Unbounded_String
      is (F.Name);

   begin
      for Field of Fields loop
         Ada_Definition_Vectors.Append (Definitions, Ada_Definition_Type'(
            Name      => To_Ada_Case (Field_Name (Field)),
            Type_Name => To_Ada_Case (Field_Type (Field)),
            Is_Access => (Field.Name /= "int")
         ));
      end loop;
   end Generate_Definitions;

   procedure Write_Definitions
      (Definitions : Ada_Definition_Vectors.Vector)
   is
      use Ada_Definition_Vectors;
      A_Definition : Ada_Definition_Type;
      Max_Width : Natural := 0;
   begin
      for Def of Definitions loop
         if Length (Def.Name) > Max_Width then
            Max_Width := Length (Def.Name);
         end if;
      end loop;

      for I in First_Index (Definitions) .. Last_Index (Definitions) loop
         A_Definition := Element (Definitions, I);
         New_Line;
         Put (A_Definition.Name);
         for I in Length (A_Definition.Name) .. Max_Width loop
            Put (" ");
         end loop;
         Put (": ");
         if A_Definition.Is_Access then
            Put ("access ");
         end if;
         Put (A_Definition.Type_Name);
         Put (";");
      end loop;
   end Write_Definitions;

   procedure Write
      (P : Ada_Package_Type)
   is
   begin
      Put ("package ");
      Put (P.Name);
      Put (" is");
      New_Line;
      Indent;

      New_Line;
      Put ("subtype Identifier is String;");
      New_Line;
      Put ("subtype Int is Integer;");
      New_Line;

      for A_Enum of P.Enums loop
         New_Line;
         Put ("type ");
         Put (A_Enum.Name);
         Put (" is");
         Indent;
         New_Line;
         declare
            use Ada_Enum_Value_Vectors;
         begin
            for I in First_Index (A_Enum.Values) .. Last_Index (A_Enum.Values) loop
               if I = First_Index (A_Enum.Values) then
                  Put ("(");
               else
                  Put (" ");
               end if;

               Put (A_Enum.Values (I));

               if I /= Last_Index (A_Enum.Values) then
                  Put (",");
                  New_Line;
               else
                  Put (");");
               end if;
            end loop;
         end;
         Dedent;
         New_Line;
      end loop;
      New_Line;

      for A_Record of P.Records loop
         Put ("type ");
         Put (A_Record.Name);
         Put (";");
         New_Line;
      end loop;

      for A_Record of P.Records loop
         New_Line;
         Put ("type ");
         Put (A_Record.Name);
         Put (" ");
         if not Ada_Discriminant_Vectors.Is_Empty (A_Record.Discriminants) then
            Put ("(Kind : ");
            Put (A_Record.Name);
            Put ("_Kind");
            Put (") ");
         end if;
         Put ("is record");
         Indent;

         Write_Definitions (A_Record.Definitions);

         if not Ada_Discriminant_Vectors.Is_Empty (A_Record.Discriminants) then
            New_Line;
            Put ("case Kind is");
            Indent;
            for Discriminant of A_Record.Discriminants loop
               New_Line;
               Put ("when ");
               Put (Discriminant.Name);
               Put (" =>");
               if not Ada_Definition_Vectors.Is_Empty (Discriminant.Definitions) then
                  Indent;
                  Write_Definitions (Discriminant.Definitions);
                  Dedent;
               else
                  Indent;
                  New_Line;
                  Put ("null;");
                  Dedent;
               end if;
            end loop;
            Dedent;
            New_Line;
            Put ("end case;");
         end if;
         Dedent;
         New_Line;
         Put ("end record;");
         New_Line;
      end loop;

      Dedent;
      New_Line;
      Put ("end ");
      Put (P.Name);
      Put (";");
      New_Line;
   end Write;

   procedure Generate
      (M : ASDL.Parser.ASDL_Module)
   is
      use type Ada.Containers.Count_Type;
      use ASDL.Parser.ASDL_Constructor_Vectors;
      A_Package : Ada_Package_Type;
      A_Record  : Ada_Record_Type;
      A_Discriminant : Ada_Discriminant_Type;
      A_Enum : Ada_Enum_Type;
   begin
      A_Package.Name := To_Ada_Case (M.Name);
      for Def of M.Defs loop
         Ada_Definition_Vectors.Clear (A_Discriminant.Definitions);
         Ada_Definition_Vectors.Clear (A_Record.Definitions);
         Ada_Discriminant_Vectors.Clear (A_Record.Discriminants);

         A_Record.Name := To_Ada_Case (Def.Name);
         case Def.Typ.Kind is
            when ASDL.Parser.Product =>
               Generate_Definitions (A_Record.Definitions, Def.Typ.Fields);
               Ada_Record_Vectors.Append (A_Package.Records, A_Record);
            when ASDL.Parser.Sum =>
               A_Enum.Name := A_Record.Name & "_Kind";
               Ada_Enum_Value_Vectors.Clear (A_Enum.Values);

               if Length (Def.Typ.Constructors) = 1 then
                  Generate_Definitions (A_Record.Definitions, First_Element (Def.Typ.Constructors).Fields);
               else
                  for Constructor of Def.Typ.Constructors loop
                     Ada_Definition_Vectors.Clear (A_Discriminant.Definitions);
                     A_Discriminant.Name := Constructor.Name;
                     Generate_Definitions (A_Discriminant.Definitions, Constructor.Fields);
                     Ada_Discriminant_Vectors.Append (A_Record.Discriminants, A_Discriminant);
                     Ada_Enum_Value_Vectors.Append (A_Enum.Values, Constructor.Name);
                  end loop;
                  Ada_Enum_Vectors.Append (A_Package.Enums, A_Enum);
               end if;

               Generate_Definitions (A_Record.Definitions, Def.Typ.Attributes);
               Ada_Record_Vectors.Append (A_Package.Records, A_Record);
         end case;
      end loop;

      Write (A_Package);
   end Generate;
end ASDL.Codegen;
