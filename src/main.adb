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
with WACC.TACKY;
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
      --  Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, AAA.Strings.Flatten (Args));
      Status := AAA.Processes.Run (Command_Line => Args);
   end Preprocess;

   type Compile_Stage is (Lex, Parse, Tacky, Codegen);

   procedure Compile
      (Preprocessed_File, Assembly_File : String;
       Stage : Compile_Stage)
   is
      Tokens : WACC.Lexer.Token_List;
      Tree   : WACC.AST.Program_Node;
      TAC    : WACC.TACKY.Program_Node;
      Asm    : WACC.Assembly.Program_Node;
   begin
      for S in Compile_Stage'First .. Stage loop
         case S is
            when Lex =>
               WACC.Lexer.Lex (Preprocessed_File, Tokens);
            when Parse =>
               WACC.Parser.Parse_Program (Tokens, Tree);
               --  WACC.AST.Print (Tree);
            when Tacky =>
               WACC.TACKY.Generate (Tree, TAC);
               --  WACC.TACKY.Print (TAC);
            when Codegen =>
               WACC.Codegen.Generate (Tree, Asm);
               WACC.Assembly.Print (Asm, Assembly_File);
         end case;
      end loop;
   end Compile;

   procedure Assemble
      (Assembly_File, Object_File : String)
   is
      use type AAA.Strings.Vector;
      Args : constant AAA.Strings.Vector := AAA.Strings.Empty_Vector &
         "gcc" & "-c" & Assembly_File & "-o" & Object_File;
      Status : AAA.Processes.Result with Unreferenced;
   begin
      --  Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, AAA.Strings.Flatten (Args));
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
      --  Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, AAA.Strings.Flatten (Args));
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
   Stage : Compile_Stage := Compile_Stage'Last;
begin
   CLI.Set_Exit_Status (0);
   for I in 1 .. CLI.Argument_Count loop
      declare
         Arg : constant String := CLI.Argument (I);
      begin
         if Arg'Length > 2 and then Arg (1 .. 2) = "--" then
            if Arg (3 .. Arg'Last) = "lex" then
               Stage := Lex;
            elsif Arg (3 .. Arg'Last) = "parse" then
               Stage := Parse;
            elsif Arg (3 .. Arg'Last) = "tacky" then
               Stage := Tacky;
            elsif Arg (3 .. Arg'Last) = "codegen" then
               Stage := Codegen;
            end if;
         else
            Input_File_Arg := I;
         end if;
      end;
   end loop;

   if Input_File_Arg = 0 then
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
         "Usage: wacc" &
         " [--lex] [--parse] [--tacky] [--codegen] " &
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
      Compile (Preprocessed_File, Assembly_File, Stage);
      Ada.Directories.Delete_File (Preprocessed_File);
      if Stage = Compile_Stage'Last then
         Assemble (Assembly_File, Object_File);
         Ada.Directories.Delete_File (Assembly_File);
         Link (Object_File, Executable_File);
         Ada.Directories.Delete_File (Object_File);
      end if;
   exception
      when E : AAA.Processes.Child_Error | WACC.Lexer.Lex_Error | WACC.Parser.Parse_Error =>
         Delete_If_Exists (Preprocessed_File);
         Delete_If_Exists (Assembly_File);
         Delete_If_Exists (Object_File);
         Delete_If_Exists (Executable_File);
         CLI.Set_Exit_Status (2);
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
            Ada.Exceptions.Exception_Information (E));
      when E : others =>
         CLI.Set_Exit_Status (3);
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
            Ada.Exceptions.Exception_Information (E));
   end;
end Main;
