private with Ada.Containers.Vectors;

package WACC.Lexer
   with Elaborate_Body, SPARK_Mode => On
is

   type Token_Type is (T_Identifier, T_int);
   type Token
      (Typ : Token_Type)
   is null record;

   type Token_List is private;

   procedure Lex
      (Input_File : String;
       Tokens     : out Token_List);

private

   type Token_List is null record;

end WACC.Lexer;
