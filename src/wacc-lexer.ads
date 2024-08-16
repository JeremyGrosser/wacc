pragma Style_Checks ("M120");
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package WACC.Lexer
   with Elaborate_Body
is
   Lex_Error : exception;

   type Token_Type is
      (T_Identifier,
       T_Constant,
       T_int,
       T_void,
       T_return,
       T_if,
       T_else,
       T_goto,
       T_Open_Paren,
       T_Close_Paren,
       T_Open_Brace,
       T_Close_Brace,
       T_Semicolon,
       T_Dash_Dash,
       T_Tilde,
       T_Bang,
       T_Colon,

       T_Dash,
       T_Plus,
       T_Asterisk,
       T_Slash,
       T_Percent,
       T_And_And,
       T_Pipe_Pipe,
       T_Question,
       T_Equal_Equal,
       T_Bang_Equal,
       T_Less_Than,
       T_Greater_Than,
       T_Less_Equal,
       T_Greater_Equal,
       T_Equal);

   subtype Binary_Operator_Token_Type is Token_Type range T_Dash .. T_Equal;

   type Token is record
      Typ : Token_Type;
      Literal : Unbounded_String := Null_Unbounded_String;
   end record;

   package Token_Vectors is new Ada.Containers.Vectors (Positive, Token);
   subtype Token_List is Token_Vectors.Vector;

   procedure Lex
      (Input_File : String;
       Tokens     : out Token_List);

end WACC.Lexer;
