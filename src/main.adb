pragma Ada_2022;
pragma Style_Checks ("M120");
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Directories;

with AAA.Strings;
with AAA.Processes;

with WACC.Lexer;
with WACC.AST;
with WACC.Parser;
with WACC.Assembly;
with WACC.Codegen;

procedure Main is
   package CLI renames Ada.Command_Line;

   procedure Preprocess
      (Input_File, Preprocessed_File : String)
   is
      use type AAA.Strings.Vector;
      Args : constant AAA.Strings.Vector := AAA.Strings.Empty_Vector &
         "gcc" & "-E" & "-P" & Input_File & "-o" & Preprocessed_File;
      Status : AAA.Processes.Result with Unreferenced;
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, AAA.Strings.Flatten (Args));
      Status := AAA.Processes.Run (Command_Line => Args);
   end Preprocess;

   procedure Compile
      (Preprocessed_File, Assembly_File : String;
       Lex, Parse, Codegen : Boolean)
   is
      Tokens : WACC.Lexer.Token_List;
      Tree   : WACC.AST.Program_Node;
      Asm    : WACC.Assembly.Program_Node;
   begin
      if Lex or else Parse or else Codegen then
         WACC.Lexer.Lex (Preprocessed_File, Tokens);
      end if;

      if Parse or else Codegen then
         WACC.Parser.Parse_Program (Tokens, Tree);
         --  WACC.AST.Print (Tree);
      end if;

      if Codegen then
         WACC.Codegen.Generate (Tree, Asm);
         WACC.Assembly.Print (Asm, Assembly_File);
      end if;
   end Compile;

   procedure Assemble
      (Assembly_File, Object_File : String)
   is
      use type AAA.Strings.Vector;
      Args : constant AAA.Strings.Vector := AAA.Strings.Empty_Vector &
         "gcc" & "-c" & Assembly_File & "-o" & Object_File;
      Status : AAA.Processes.Result with Unreferenced;
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, AAA.Strings.Flatten (Args));
      Status := AAA.Processes.Run (Command_Line => Args);
   end Assemble;

   procedure Link
      (Object_File, Executable_File : String)
   is
      use type AAA.Strings.Vector;
      Args : constant AAA.Strings.Vector := AAA.Strings.Empty_Vector &
         "gcc" & Object_File & "-o" & Executable_File;
      Status : AAA.Processes.Result with Unreferenced;
   begin
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, AAA.Strings.Flatten (Args));
      Status := AAA.Processes.Run (Command_Line => Args);
   end Link;

   procedure Delete_If_Exists
      (File : String)
   is
   begin
      if Ada.Directories.Exists (File) then
         Ada.Directories.Delete_File (File);
      end if;
   end Delete_If_Exists;

   Input_File_Arg : Natural := 0;
   Should_Lex, Should_Parse, Should_Codegen : Boolean := False;
begin
   CLI.Set_Exit_Status (0);
   for I in 1 .. CLI.Argument_Count loop
      declare
         Arg : constant String := CLI.Argument (I);
      begin
         if Arg'Length > 2 and then Arg (1 .. 2) = "--" then
            if Arg (3 .. Arg'Last) = "lex" then
               Should_Lex := True;
            elsif Arg (3 .. Arg'Last) = "parse" then
               Should_Parse := True;
            elsif Arg (3 .. Arg'Last) = "codegen" then
               Should_Codegen := True;
            end if;
         else
            Input_File_Arg := I;
         end if;
      end;
   end loop;

   if Input_File_Arg = 0 then
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
         "Usage: wacc" &
         " [--lex] [--parse] [--codegen] " &
         "<input file>");
      CLI.Set_Exit_Status (1);
      return;
   end if;

   declare
      Input_File : constant String := CLI.Argument (Input_File_Arg);
      Basename : constant String := AAA.Strings.Split
         (Text      => Input_File,
          Separator => '.',
          From      => AAA.Strings.Tail);
      Preprocessed_File : constant String := Basename & ".i";
      Assembly_File : constant String := Basename & ".s";
      Object_File : constant String := Basename & ".o";
      Executable_File : constant String := Basename;
   begin
      Preprocess (Input_File, Preprocessed_File);
      Compile (Preprocessed_File, Assembly_File,
         Lex      => Should_Lex,
         Parse    => Should_Parse,
         Codegen  => Should_Codegen);
      Ada.Directories.Delete_File (Preprocessed_File);
      Assemble (Assembly_File, Object_File);
      Ada.Directories.Delete_File (Assembly_File);
      Link (Object_File, Executable_File);
      Ada.Directories.Delete_File (Object_File);
   exception
      when E : AAA.Processes.Child_Error | WACC.Lexer.Lex_Error | WACC.Parser.Parse_Error =>
         Delete_If_Exists (Preprocessed_File);
         Delete_If_Exists (Assembly_File);
         Delete_If_Exists (Object_File);
         Delete_If_Exists (Executable_File);
         CLI.Set_Exit_Status (2);
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
            Ada.Exceptions.Exception_Message (E));
      when E : others =>
         CLI.Set_Exit_Status (3);
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
            Ada.Exceptions.Exception_Message (E));
   end;
end Main;
