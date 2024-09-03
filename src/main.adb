pragma Ada_2022;
pragma Style_Checks ("M120");
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Directories;
with Ada.Containers.Vectors;

with AAA.Strings;
with AAA.Processes;

with WACC.Lexer;
with WACC.AST;
with WACC.Parser;
with WACC.Semantic_Analysis;
with WACC.TACKY;
with WACC.Assembly;

procedure Main is
   package CLI renames Ada.Command_Line;

   type Compile_Stage is (Lex, Parse, Validate, Tacky, Codegen, Object, Link);

   procedure Exec
      (Cmd : AAA.Strings.Vector);
   procedure Preprocess
      (Input_File, Preprocessed_File : String);
   procedure Compile
      (Preprocessed_File, Assembly_File : String;
       Stage : Compile_Stage);
   procedure Assemble
      (Assembly_File, Object_File : String);

   procedure Exec
      (Cmd : AAA.Strings.Vector)
   is
      Status : AAA.Processes.Result;
   begin
      Status := AAA.Processes.Run
         (Command_Line     => Cmd,
          Err_To_Out       => True,
          Raise_On_Error   => False);
      if Status.Exit_Code /= 0 then
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, AAA.Strings.Flatten (Cmd));
         for O of Status.Output loop
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, O);
         end loop;
         raise Program_Error;
      end if;
   end Exec;

   procedure Preprocess
      (Input_File, Preprocessed_File : String)
   is
      use type AAA.Strings.Vector;
   begin
      Exec (AAA.Strings.Empty_Vector &
         "gcc" & "-E" & "-P" & Input_File & "-o" & Preprocessed_File);
   end Preprocess;

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
               --  WACC.Lexer.Print (Tokens);
            when Parse =>
               WACC.Parser.Parse_Program (Tokens, Tree);
               --  WACC.AST.Print (Tree);
            when Validate =>
               WACC.Semantic_Analysis.Analyze (Tree);
               --  WACC.AST.Print (Tree);
            when Tacky =>
               WACC.TACKY.Generate (Tree, TAC);
               --  WACC.TACKY.Print (TAC);
            when Codegen =>
               WACC.Assembly.Generate (TAC, Asm);
               --  WACC.Assembly.Print (Asm);
            when Object | Link =>
               WACC.Assembly.Emit (Asm, Assembly_File);
         end case;
      end loop;
   end Compile;

   procedure Assemble
      (Assembly_File, Object_File : String)
   is
      use type AAA.Strings.Vector;
   begin
      Exec (AAA.Strings.Empty_Vector &
         "gcc" & "-c" & Assembly_File & "-o" & Object_File);
   end Assemble;

   function Source_File
      (Arg : Natural)
      return String
   is (CLI.Argument (Arg));

   function Basename
      (Arg : Natural)
      return String
   is (AAA.Strings.Split
         (Text      => Source_File (Arg),
          Separator => '.',
          From      => AAA.Strings.Tail));

   function Preprocessed_File
      (Arg : Natural)
      return String
   is (Basename (Arg) & ".i");

   function Assembly_File
      (Arg : Natural)
      return String
   is (Basename (Arg) & ".s");

   function Object_File
      (Arg : Natural)
      return String
   is (Basename (Arg) & ".o");

   package Natural_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Positive,
       Element_Type => Natural);
   use Natural_Vectors;
   Input_File_Args : Natural_Vectors.Vector := Natural_Vectors.Empty_Vector;
   Stage : Compile_Stage := Compile_Stage'Last;
   Keep_Files : Boolean := False;
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
            elsif Arg (3 .. Arg'Last) = "validate" then
               Stage := Validate;
            elsif Arg (3 .. Arg'Last) = "tacky" then
               Stage := Tacky;
            elsif Arg (3 .. Arg'Last) = "codegen" then
               Stage := Codegen;
            elsif Arg (3 .. Arg'Last) = "keep" then
               Keep_Files := True;
            end if;
         elsif Arg'Length = 2 and then Arg (1) = '-' then
            case Arg (2) is
               when 'c' =>
                  Stage := Object;
               when others =>
                  raise Program_Error with "Unknown option: " & Arg (2);
            end case;
         else
            Append (Input_File_Args, I);
         end if;
      end;
   end loop;

   if Is_Empty (Input_File_Args) then
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
         "Usage: wacc" &
         " [-c] [--lex] [--parse] [--validate] [--tacky] [--codegen] " &
         "<input file>");
      CLI.Set_Exit_Status (1);
      return;
   end if;

   for Arg of Input_File_Args loop
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "    WACC " & Source_File (Arg));
      Preprocess (Source_File (Arg), Preprocessed_File (Arg));
      Compile (Preprocessed_File (Arg), Assembly_File (Arg), Stage);
      if not Keep_Files then
         Ada.Directories.Delete_File (Preprocessed_File (Arg));
      end if;

      if Stage in Object .. Link then
         Assemble (Assembly_File (Arg), Object_File (Arg));
         if not Keep_Files then
            Ada.Directories.Delete_File (Assembly_File (Arg));
         end if;
      end if;
   end loop;

   if Stage = Link then
      declare
         use AAA.Strings;
         V : AAA.Strings.Vector := AAA.Strings.Empty_Vector & "gcc";
         Executable_File : constant String := Basename (First_Element (Input_File_Args));
      begin
         for Arg of Input_File_Args loop
            Append (V, Object_File (Arg));
         end loop;
         Append (V, "-o");
         Append (V, Executable_File);
         Exec (V);
      end;

      if not Keep_Files then
         for Arg of Input_File_Args loop
            Ada.Directories.Delete_File (Object_File (Arg));
         end loop;
      end if;
   end if;
exception
   when E : others =>
      CLI.Set_Exit_Status (2);
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
         Ada.Exceptions.Exception_Information (E));
end Main;
