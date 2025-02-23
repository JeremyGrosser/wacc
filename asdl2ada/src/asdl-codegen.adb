pragma Style_Checks ("M120");
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Integer_Text_IO;
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

   procedure Put
      (I : Integer)
   is
   begin
      Ada.Integer_Text_IO.Put (I, Width => 0);
   end Put;

   package String_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type        => String,
       Element_Type    => Positive,
       Hash            => Ada.Strings.Hash,
       Equivalent_Keys => "=");

   Sequences : String_Maps.Map := String_Maps.Empty_Map;

   procedure Make_Sequence
      (Fields : ASDL.Parser.ASDL_Field_Vectors.Vector)
   is
      use type ASDL.Parser.ASDL_Qualifier;
      use String_Maps;
   begin
      for Field of Fields loop
         declare
            Name : constant String := To_String (Field.Name);
         begin
            if Field.Qualifier = ASDL.Parser.Sequence and then
               not Contains (Sequences, Name)
            then
               Insert (Sequences, Name, 1);
               Put ("package ");
               Put (Name);
               Put ("_Vectors is new Ada.Containers.Indefinite_Vectors");
               Indent;
               New_Line;
               Put ("(Positive, ");
               Put (Name);
               Put (");");
               Dedent;
               New_Line;
            end if;
         end;
      end loop;
   end Make_Sequence;

   procedure Make_Sequences
      (Defs : ASDL.Parser.ASDL_Definition_Vectors.Vector)
   is
   begin
      for Def of Defs loop
         case Def.Typ.Kind is
            when ASDL.Parser.Sum =>
               for Constructor of Def.Typ.Constructors loop
                  Make_Sequence (Constructor.Fields);
               end loop;
               Make_Sequence (Def.Typ.Attributes);
            when ASDL.Parser.Product =>
               Make_Sequence (Def.Typ.Fields);
         end case;
      end loop;
   end Make_Sequences;

   procedure Generate
      (F : ASDL.Parser.ASDL_Field_Vectors.Vector)
   is
      use type ASDL.Parser.ASDL_Qualifier;

      use String_Maps;
      Count : String_Maps.Map := Empty_Map;

      procedure Increment
         (N : String)
      is
         X : Positive;
      begin
         X := Element (Count, N);
         Delete (Count, N);
         Insert (Count, N, X + 1);
      end Increment;
   begin
      for Field of F loop
         New_Line;
         declare
            Name : constant String :=
               (if Field.Id /= Null_Unbounded_String then
                  To_String (Field.Id)
                else
                  To_String (Field.Name));
            Typ : constant String := To_String (Field.Name);
         begin
            if not Contains (Count, Name) then
               Insert (Count, Name, 1);
            end if;

            Put (Name);
            if Name = Typ then
               Put ("_");
               Put (Element (Count, Name));
               Increment (Name);
            end if;

            Put (" : ");
            if Typ = "string" then
               Put ("A_string");
            else
               Put (Typ);
            end if;
            if Field.Qualifier = ASDL.Parser.Sequence then
               Put ("_Vectors.Vector");
            end if;
            Put (";");

            if Field.Qualifier = ASDL.Parser.Optional then
               Put (" --  optional");
               New_Line;
               Put ("Has_");
               Put (Name);
               Put (" : Boolean := False;");
            end if;
         end;
      end loop;
   end Generate;

   procedure Generate
      (M : ASDL.Parser.ASDL_Module)
   is
      use ASDL.Parser.ASDL_Field_Vectors;
      use type ASDL.Parser.ASDL_Type_Kind;
   begin
      Put ("pragma Style_Checks (Off);");
      New_Line;
      Put ("with Ada.Containers.Indefinite_Vectors;");
      New_Line;
      Put ("with Ada.Strings.Unbounded;");
      New_Line;
      New_Line;

      Put ("package ");
      Put (M.Name);
      Put (" is");
      Indent;
      New_Line;

      Put ("subtype identifier is Ada.Strings.Unbounded.Unbounded_String;");
      New_Line;
      Put ("subtype A_string is Ada.Strings.Unbounded.Unbounded_String;");
      New_Line;
      Put ("subtype int is Integer;");
      New_Line;
      New_Line;

      for Def of M.Defs loop
         Put ("type ");
         Put (Def.Name);
         Put (" is tagged ");
         if Def.Typ.Kind = ASDL.Parser.Sum and then
            not ASDL.Parser.ASDL_Field_Vectors.Is_Empty (Def.Typ.Attributes)
         then
            Put ("record");
            Indent;
            Generate (Def.Typ.Attributes);
            Dedent;
            New_Line;
            Put ("end record;");
         else
            Put ("null record;");
         end if;
         New_Line;
      end loop;

      New_Line;

      Make_Sequences (M.Defs);

      New_Line;

      for Def of M.Defs loop
         case Def.Typ.Kind is
            when ASDL.Parser.Sum =>
               for Constructor of Def.Typ.Constructors loop
                  Put ("type ");
                  Put (Constructor.Name);
                  Put (" is new ");
                  Put (Def.Name);
                  Put (" with ");
                  if Is_Empty (Constructor.Fields) then
                     Put ("null record;");
                     New_Line;
                     New_Line;
                  else
                     Put ("record");
                     Indent;
                     Generate (Constructor.Fields);
                     Dedent;
                     New_Line;
                     Put ("end record;");
                     New_Line;
                     New_Line;
                  end if;
               end loop;
            when ASDL.Parser.Product =>
               null;
         end case;
      end loop;

      Dedent;
      New_Line;
      Put ("end ");
      Put (M.Name);
      Put (";");
      New_Line;
   end Generate;
end ASDL.Codegen;
