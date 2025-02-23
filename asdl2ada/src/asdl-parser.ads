with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;

package ASDL.Parser is

   type ASDL_Qualifier is (None, Sequence, Optional);

   type ASDL_Field is record
      Name      : Unbounded_String;
      Qualifier : ASDL_Qualifier;
      Id        : Unbounded_String;
   end record;

   package ASDL_Field_Vectors is new Ada.Containers.Vectors
      (Positive, ASDL_Field);

   type ASDL_Constructor is record
      Name   : Unbounded_String;
      Fields : ASDL_Field_Vectors.Vector;
   end record;

   package ASDL_Constructor_Vectors is new Ada.Containers.Vectors
      (Positive, ASDL_Constructor);

   type ASDL_Type_Kind is (Product, Sum);

   type ASDL_Type
      (Kind : ASDL_Type_Kind)
   is record
      case Kind is
         when Sum =>
            Constructors : ASDL_Constructor_Vectors.Vector;
            Attributes : ASDL_Field_Vectors.Vector;
         when Product =>
            Fields : ASDL_Field_Vectors.Vector;
      end case;
   end record;

   type Any_ASDL_Type is access ASDL_Type;

   type ASDL_Definition is record
      Name : Unbounded_String;
      Typ  : Any_ASDL_Type;
   end record;

   package ASDL_Definition_Vectors is new Ada.Containers.Indefinite_Vectors
      (Positive, ASDL_Definition);

   type ASDL_Module is record
      Name : Unbounded_String;
      Defs : ASDL_Definition_Vectors.Vector;
   end record;

   procedure Parse
      (Text   : String;
       Module : out ASDL_Module);

end ASDL.Parser;
