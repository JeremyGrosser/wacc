package body ASDL.Lexer is
   procedure Scan
      (Text   : String;
       Tokens : out Token_Vectors.Vector)
   is
      First : Natural := Text'First;
      Last  : Natural := First - 1;
      Line_Number : Positive := 1;
      Line_First : Natural := Text'First;

      function End_Of_Input
         return Boolean
      is (First >= Text'Last);

      procedure Scan_Alpha_Num is
      begin
         Last := First + 1;
         loop
            if Last >= Text'Last or else
               (Text (Last) not in 'a' .. 'z' and then
                Text (Last) not in 'A' .. 'Z' and then
                Text (Last) not in '0' .. '9' and then
                Text (Last) /= '_')
            then
               Last := Last - 1;
               exit;
            else
               Last := Last + 1;
            end if;
         end loop;
      end Scan_Alpha_Num;

      procedure Add_Token
         (Kind : Token_Kind)
      is
         T : constant Token :=
            (Kind  => Kind,
             First => First,
             Last  => Last,
             Line  => Line_Number);
      begin
         Token_Vectors.Append (Tokens, T);
         First := Last + 1;
      end Add_Token;
   begin
      Token_Vectors.Clear (Tokens);
      loop
         exit when End_Of_Input;
         case Text (First) is
            when 'a' .. 'z' =>
               Scan_Alpha_Num;
               Add_Token (Type_Id);
            when 'A' .. 'Z' =>
               Scan_Alpha_Num;
               Add_Token (Constructor_Id);
            when '=' =>
               Last := First;
               Add_Token (Equals);
            when ',' =>
               Last := First;
               Add_Token (Comma);
            when '?' =>
               Last := First;
               Add_Token (Question);
            when '*' =>
               Last := First;
               Add_Token (Asterisk);
            when '|' =>
               Last := First;
               Add_Token (Pipe);
            when '(' =>
               Last := First;
               Add_Token (LParen);
            when ')' =>
               Last := First;
               Add_Token (RParen);
            when '{' =>
               Last := First;
               Add_Token (LBrace);
            when '}' =>
               Last := First;
               Add_Token (RBrace);
            when ASCII.LF | ASCII.CR =>
               Line_Number := Line_Number + 1;
               Line_First := First;
               First := First + 1;
            when ' ' | ASCII.HT =>
               First := First + 1;
            when others =>
               declare
                  Position : constant Natural := First - Line_First;
               begin
                  raise Program_Error with
                     "Unexpected character on line" & Line_Number'Image &
                     " position" & Position'Image &
                     ": '" & Text (First) & "'";
               end;
         end case;
      end loop;
   end Scan;

end ASDL.Lexer;
