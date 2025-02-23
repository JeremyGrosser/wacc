pragma Style_Checks ("M120");
with ASDL.Lexer; use ASDL.Lexer;

package body ASDL.Parser is

   procedure Parse
      (Text   : String;
       Module : out ASDL_Module)
   is
      Tokens : Token_Vectors.Vector;
      Index  : Positive := 1;

      function Current
         return Token
      is (Tokens (Index));

      function Value
         return String
      is (Text (Current.First .. Current.Last));

      procedure Advance is
      begin
         Index := Index + 1;
      end Advance;

      function At_Keyword
         (Keyword : String)
         return Boolean
      is (Current.Kind = Type_Id and then Value = Keyword);

      procedure Expect
         (Kind : Token_Kind)
      is
      begin
         if Current.Kind /= Kind then
            raise Program_Error with "Expected " & Kind'Image &
                                     ", got """ & Value & """";
         else
            Advance;
         end if;
      end Expect;

      procedure Parse_Fields
         (Fields : out ASDL_Field_Vectors.Vector)
      is
         Field : ASDL_Field;
      begin
         ASDL_Field_Vectors.Clear (Fields);
         Expect (LParen);
         loop
            exit when Current.Kind /= Type_Id;
            Field.Name := To_Unbounded_String (Value);
            Advance;
            case Current.Kind is
               when Question =>
                  Field.Qualifier := Optional;
                  Advance;
               when Asterisk =>
                  Field.Qualifier := Sequence;
                  Advance;
               when others =>
                  Field.Qualifier := None;
            end case;

            if Current.Kind in Constructor_Id .. Type_Id then
               Field.Id := To_Unbounded_String (Value);
               Advance;
            end if;
            ASDL_Field_Vectors.Append (Fields, Field);
            exit when Current.Kind = RParen;
            Expect (Comma);
         end loop;
         Expect (RParen);
      end Parse_Fields;

      procedure Parse_Optional_Fields
         (Fields : out ASDL_Field_Vectors.Vector)
      is
      begin
         ASDL_Field_Vectors.Clear (Fields);
         if Current.Kind = LParen then
            Parse_Fields (Fields);
         end if;
      end Parse_Optional_Fields;

      procedure Parse_Product
         (Typ : out Any_ASDL_Type)
      is
      begin
         Typ := new ASDL_Type (Kind => Product);
         Parse_Fields (Typ.Fields);
      end Parse_Product;

      procedure Parse_Type
         (Typ : out Any_ASDL_Type)
      is
         Constructor : ASDL_Constructor;
      begin
         if Current.Kind = LParen then
            Parse_Product (Typ);
         else
            Typ := new ASDL_Type (Kind => Sum);
            loop
               if Current.Kind = Constructor_Id then
                  Constructor.Name := To_Unbounded_String (Value);
                  Advance;
                  Parse_Optional_Fields (Constructor.Fields);
               else
                  raise Program_Error with "Expected Constructor, got " & Value;
               end if;
               ASDL_Constructor_Vectors.Append (Typ.Constructors, Constructor);
               exit when Current.Kind /= Pipe;
               Advance;
            end loop;
         end if;
      end Parse_Type;

      procedure Parse_Definitions
         (Defs : out ASDL_Definition_Vectors.Vector)
      is
         Def : ASDL_Definition;
      begin
         loop
            exit when Current.Kind /= Type_Id;
            if At_Keyword ("attributes") then
               Advance;
               Def := ASDL_Definition_Vectors.Last_Element (Defs);
               ASDL_Definition_Vectors.Delete_Last (Defs);
               Parse_Fields (Def.Typ.Attributes);
            else
               Def.Name := To_Unbounded_String (Value);
               Advance;
               Expect (Equals);
               Parse_Type (Def.Typ);
            end if;
            ASDL_Definition_Vectors.Append (Defs, Def);
         end loop;
      end Parse_Definitions;

      procedure Parse_Module
         (M : out ASDL_Module)
      is
      begin
         if At_Keyword ("module") then
            Advance;
         else
            raise Program_Error with "Expected ""module"", found """ & Value & """";
         end if;

         if Current.Kind in Constructor_Id .. Type_Id then
            M.Name := To_Unbounded_String (Value);
            Advance;
            Expect (LBrace);
            Parse_Definitions (M.Defs);
            Expect (RBrace);
         else
            raise Program_Error with "Expected Constructor_Id or Type_Id, got " & Value;
         end if;
      end Parse_Module;
   begin
      ASDL.Lexer.Scan (Text, Tokens);
      Parse_Module (Module);
   end Parse;

end ASDL.Parser;
