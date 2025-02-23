with Ada.Containers.Indefinite_Vectors;

package ASDL.Lexer is

   type Token_Kind is
      (Constructor_Id,
       Type_Id,
       Equals,
       Comma,
       Question,
       Pipe,
       Asterisk,
       LParen,
       RParen,
       LBrace,
       RBrace);

   type Token is record
      Kind : Token_Kind;
      First, Last : Natural;
      Line : Positive;
   end record;

   package Token_Vectors is new Ada.Containers.Indefinite_Vectors
      (Positive, Token);

   procedure Scan
      (Text   : String;
       Tokens : out Token_Vectors.Vector);

end ASDL.Lexer;
