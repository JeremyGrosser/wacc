pragma Style_Checks ("M120");
with Ada.Strings.Unbounded;
with Ada.Containers;
with WACC.Strings;

package body WACC.Parser is

   procedure Parse_Program
      (Tokens : WACC.Lexer.Token_List;
       Tree   : out WACC.AST.Program_Node)
   is
      use type WACC.Lexer.Token_Type;
      use WACC.Lexer.Token_Vectors;
      Input : WACC.Lexer.Token_List := Tokens;

      type Operator_Precedence is range 0 .. 51;

      --  Grammar
      procedure Parse_Declaration
         (Node : out WACC.AST.Any_Declaration_Node);
      procedure Parse_Variable_Declaration
         (Node : out WACC.AST.Any_Variable_Declaration_Node);
      procedure Parse_Function_Declaration
         (Node : out WACC.AST.Any_Function_Declaration_Node);
      procedure Parse_Specifiers
         (Storage_Class : out WACC.AST.Any_Storage_Class_Node); -- TODO: output decl type other than int
      procedure Parse_Param_List
         (Node : in out WACC.AST.Identifier_Vectors.Vector);
      procedure Parse_Block
         (Node : out WACC.AST.Any_Block_Node);
      procedure Parse_Block_Item
         (Node : out WACC.AST.Any_Block_Item_Node);
      procedure Parse_For_Init
         (Node : out WACC.AST.Any_For_Init_Node);
      procedure Parse_Statement
         (Node : out WACC.AST.Any_Statement_Node);
      procedure Parse_Exp
         (Left : out WACC.AST.Any_Exp_Node;
          Min_Precedence : Operator_Precedence := Operator_Precedence'First);
      procedure Parse_Factor
         (Node : out WACC.AST.Any_Exp_Node);
      procedure Parse_Argument_List
         (Node : in out WACC.AST.Exp_Node_Vectors.Vector);
      procedure Parse_Unop
         (Node : out WACC.AST.Any_Unary_Operator_Node);
      procedure Parse_Binop
         (Node : out WACC.AST.Any_Binary_Operator_Node);
      function Parse_Int
         (Str : String)
         return Long_Integer;

      --  Token manipulation
      function Error_Token
         (Message : String)
         return WACC.Lexer.Token;
      function Next_Token
         return WACC.Lexer.Token;
      function Peek_Token (N : Positive := 1)
         return WACC.Lexer.Token;
      procedure Expect
         (Typ : WACC.Lexer.Token_Type);

      --  Expression helpers
      function Precedence
         (Typ : WACC.Lexer.Binary_Operator_Token_Type)
         return Operator_Precedence;
      function Parse_Condition_Middle
         return WACC.AST.Any_Exp_Node;
      procedure Parse_Optional_Exp
         (Node : out WACC.AST.Any_Exp_Node;
          Stop : WACC.Lexer.Token_Type);

      --  Statement helpers
      procedure Parse_For
         (Node : out WACC.AST.Any_Statement_Node);

      ----------------------------------------

      function Error_Token
         (Message : String)
         return WACC.Lexer.Token
      is (WACC.Lexer.Token'(WACC.Lexer.T_Error, Ada.Strings.Unbounded.To_Unbounded_String (Message)));

      function Next_Token
         return WACC.Lexer.Token
      is (if not Is_Empty (Input) then First_Element (Input) else Error_Token ("No more tokens in input"));

      function Peek_Token
         (N : Positive := 1)
         return WACC.Lexer.Token
      is (Element (Input, First_Index (Input) + N));

      procedure Expect
         (Typ : WACC.Lexer.Token_Type)
      is
      begin
         if Next_Token.Typ = Typ then
            Delete_First (Input);
         else
            raise Parse_Error with "Expected token " & Typ'Image & ", found " &
                                   WACC.Lexer.Image (Next_Token) & " instead";
         end if;
      end Expect;

      procedure Parse_Unop
         (Node : out WACC.AST.Any_Unary_Operator_Node)
      is
      begin
         case Next_Token.Typ is
            when WACC.Lexer.T_Dash =>
               Node := new WACC.AST.Unary_Operator_Node'(Typ => WACC.AST.N_Negate);
            when WACC.Lexer.T_Tilde =>
               Node := new WACC.AST.Unary_Operator_Node'(Typ => WACC.AST.N_Complement);
            when WACC.Lexer.T_Bang =>
               Node := new WACC.AST.Unary_Operator_Node'(Typ => WACC.AST.N_Not);
            when others =>
               raise Parse_Error with "Expected Dash or Tilde token in unary operator, got " &
                  WACC.Lexer.Image (Next_Token);
         end case;
         Delete_First (Input);
      end Parse_Unop;

      function Parse_Int
         (Str : String)
         return Long_Integer
      is
         N : Long_Integer := 0;
      begin
         for Ch of Str loop
            if Ch not in '0' .. '9' then
               raise Parse_Error with "Expected 0 .. 9 in int constant, got '" & Ch & "'";
            end if;
            N := N * 10 + Long_Integer (Character'Pos (Ch) - Character'Pos ('0'));
         end loop;
         return N;
      end Parse_Int;

      procedure Parse_Binop
         (Node : out WACC.AST.Any_Binary_Operator_Node)
      is
      begin
         case Next_Token.Typ is
            when WACC.Lexer.T_Dash =>
               Node := new WACC.AST.Binary_Operator_Node'(Typ => WACC.AST.N_Subtract);
            when WACC.Lexer.T_Plus =>
               Node := new WACC.AST.Binary_Operator_Node'(Typ => WACC.AST.N_Add);
            when WACC.Lexer.T_Asterisk =>
               Node := new WACC.AST.Binary_Operator_Node'(Typ => WACC.AST.N_Multiply);
            when WACC.Lexer.T_Slash =>
               Node := new WACC.AST.Binary_Operator_Node'(Typ => WACC.AST.N_Divide);
            when WACC.Lexer.T_Percent =>
               Node := new WACC.AST.Binary_Operator_Node'(Typ => WACC.AST.N_Remainder);
            when WACC.Lexer.T_And_And =>
               Node := new WACC.AST.Binary_Operator_Node'(Typ => WACC.AST.N_And);
            when WACC.Lexer.T_Pipe_Pipe =>
               Node := new WACC.AST.Binary_Operator_Node'(Typ => WACC.AST.N_Or);
            when WACC.Lexer.T_Equal_Equal =>
               Node := new WACC.AST.Binary_Operator_Node'(Typ => WACC.AST.N_Equal);
            when WACC.Lexer.T_Bang_Equal =>
               Node := new WACC.AST.Binary_Operator_Node'(Typ => WACC.AST.N_Not_Equal);
            when WACC.Lexer.T_Less_Than =>
               Node := new WACC.AST.Binary_Operator_Node'(Typ => WACC.AST.N_Less_Than);
            when WACC.Lexer.T_Less_Equal =>
               Node := new WACC.AST.Binary_Operator_Node'(Typ => WACC.AST.N_Less_Or_Equal);
            when WACC.Lexer.T_Greater_Than =>
               Node := new WACC.AST.Binary_Operator_Node'(Typ => WACC.AST.N_Greater_Than);
            when WACC.Lexer.T_Greater_Equal =>
               Node := new WACC.AST.Binary_Operator_Node'(Typ => WACC.AST.N_Greater_Or_Equal);
            when others =>
               raise Parse_Error with "Unexpected token in binop: " & WACC.Lexer.Image (Next_Token);
         end case;
         Delete_First (Input);
      end Parse_Binop;

      function Precedence
         (Typ : WACC.Lexer.Binary_Operator_Token_Type)
         return Operator_Precedence
      is
         use WACC.Lexer;
      begin
         case Typ is
            when T_Percent | T_Slash | T_Asterisk =>
               return 50;
            when T_Dash | T_Plus =>
               return 45;
            when T_Less_Than | T_Less_Equal | T_Greater_Than | T_Greater_Equal =>
               return 35;
            when T_Equal_Equal | T_Bang_Equal =>
               return 30;
            when T_And_And =>
               return 10;
            when T_Pipe_Pipe =>
               return 5;
            when T_Question =>
               return 3;
            when T_Equal =>
               return 1;
         end case;
      end Precedence;

      function Parse_Condition_Middle
         return WACC.AST.Any_Exp_Node
      is
         Middle : WACC.AST.Any_Exp_Node;
      begin
         Expect (WACC.Lexer.T_Question);
         Parse_Exp (Middle);
         Expect (WACC.Lexer.T_Colon);
         return Middle;
      end Parse_Condition_Middle;

      procedure Parse_Exp
         (Left : out WACC.AST.Any_Exp_Node;
          Min_Precedence : Operator_Precedence := Operator_Precedence'First)
      is
         Middle, Right : WACC.AST.Any_Exp_Node;
         Operator : WACC.AST.Any_Binary_Operator_Node;
         Typ : WACC.Lexer.Token_Type;
      begin
         Parse_Factor (Left);
         loop
            Typ := Next_Token.Typ;
            exit when Typ not in WACC.Lexer.Binary_Operator_Token_Type'Range
                      or else Precedence (Typ) < Min_Precedence;
            if Typ = WACC.Lexer.T_Equal then
               Delete_First (Input);
               Parse_Exp (Right, Precedence (Typ));
               Left := new WACC.AST.Exp_Node'
                  (Typ          => WACC.AST.N_Assignment,
                   Assign_Left  => Left,
                   Assign_Right => Right);
            elsif Typ = WACC.Lexer.T_Question then
               Middle := Parse_Condition_Middle;
               Parse_Exp (Right, Precedence (Typ));
               Left := new WACC.AST.Exp_Node'
                  (Typ       => WACC.AST.N_Conditional,
                   Condition => Left,
                   If_True   => Middle,
                   If_False  => Right);
            else
               Parse_Binop (Operator);
               Parse_Exp (Right, Precedence (Typ) + 1);
               Left := new WACC.AST.Exp_Node'
                  (Typ             => WACC.AST.N_Binary,
                   Binary_Operator => Operator,
                   Left            => Left,
                   Right           => Right);
            end if;
         end loop;
      end Parse_Exp;

      procedure Parse_Argument_List
         (Node : in out WACC.AST.Exp_Node_Vectors.Vector)
      is
         use WACC.AST.Exp_Node_Vectors;
         E : WACC.AST.Any_Exp_Node;
      begin
         loop
            Parse_Exp (E);
            Append (Node, E);
            exit when Next_Token.Typ /= WACC.Lexer.T_Comma;
            Delete_First (Input);
         end loop;
      end Parse_Argument_List;

      procedure Parse_Factor
         (Node : out WACC.AST.Any_Exp_Node)
      is
      begin
         case Next_Token.Typ is
            when WACC.Lexer.T_Constant =>
               Node := new WACC.AST.Exp_Node'
                  (Typ => WACC.AST.N_Constant,
                   Int => Parse_Int (Ada.Strings.Unbounded.To_String (Next_Token.Literal)));
               Delete_First (Input);
            when WACC.Lexer.T_Identifier =>
               if Peek_Token.Typ = WACC.Lexer.T_Open_Paren then
                  Node := new WACC.AST.Exp_Node'
                     (Typ           => WACC.AST.N_Function_Call,
                      Function_Name => Next_Token.Literal,
                      Args          => WACC.AST.Exp_Node_Vectors.Empty_Vector);
                  Delete_First (Input);
                  Expect (WACC.Lexer.T_Open_Paren);
                  if Next_Token.Typ /= WACC.Lexer.T_Close_Paren then
                     Parse_Argument_List (Node.Args);
                  end if;
                  Expect (WACC.Lexer.T_Close_Paren);
               else
                  Node := new WACC.AST.Exp_Node'
                     (Typ  => WACC.AST.N_Var,
                      Name => Next_Token.Literal);
                  Delete_First (Input);
               end if;
            when WACC.Lexer.T_Dash | WACC.Lexer.T_Tilde | WACC.Lexer.T_Bang =>
               Node := new WACC.AST.Exp_Node'(Typ => WACC.AST.N_Unary, others => <>);
               Parse_Unop (Node.Unary_Operator);
               Parse_Factor (Node.Exp);
            when WACC.Lexer.T_Open_Paren =>
               Delete_First (Input);
               Parse_Exp (Node);
               Expect (WACC.Lexer.T_Close_Paren);
            when others =>
               raise Parse_Error with "Unexpected token in factor: " & WACC.Lexer.Image (Next_Token);
         end case;
      end Parse_Factor;

      procedure Parse_Optional_Exp
         (Node : out WACC.AST.Any_Exp_Node;
          Stop : WACC.Lexer.Token_Type)
      is
      begin
         if Next_Token.Typ /= Stop then
            Parse_Exp (Node);
         end if;
         Expect (Stop);
      end Parse_Optional_Exp;

      procedure Parse_For_Init
         (Node : out WACC.AST.Any_For_Init_Node)
      is
         use type WACC.AST.Declaration_Type;
         Decl : WACC.AST.Any_Declaration_Node;
      begin
         case Next_Token.Typ is
            when WACC.Lexer.T_int | WACC.Lexer.T_static | WACC.Lexer.T_extern =>
               Node := new WACC.AST.For_Init_Node'
                  (Typ  => WACC.AST.N_Init_Declaration,
                   Decl => null);
               Parse_Declaration (Decl);
               if Decl.Typ = WACC.AST.N_VarDecl then
                  Node.Decl := Decl.Variable_Declaration;
               else
                  raise Parse_Error with "Function declaration not allowed in for init";
               end if;
            when others =>
               Node := new WACC.AST.For_Init_Node'
                  (Typ => WACC.AST.N_Init_Expression,
                   Exp => null);
               Parse_Optional_Exp (Node.Exp, WACC.Lexer.T_Semicolon);
         end case;
      end Parse_For_Init;

      procedure Parse_For
         (Node : out WACC.AST.Any_Statement_Node)
      is
      begin
         Expect (WACC.Lexer.T_for);
         Expect (WACC.Lexer.T_Open_Paren);

         Node := new WACC.AST.Statement_Node'
            (Typ           => WACC.AST.N_For,
             For_Label     => WACC.Strings.Null_Identifier,
             For_Init      => null,
             For_Condition => null,
             For_Post      => null,
             For_Body      => null);

         Parse_For_Init (Node.For_Init);
         Parse_Optional_Exp (Node.For_Condition, WACC.Lexer.T_Semicolon);
         Parse_Optional_Exp (Node.For_Post, WACC.Lexer.T_Close_Paren);
         Parse_Statement (Node.For_Body);
      end Parse_For;

      procedure Parse_Statement
         (Node : out WACC.AST.Any_Statement_Node)
      is
         use type Ada.Containers.Count_Type;
      begin
         case Next_Token.Typ is
            when WACC.Lexer.T_return =>
               Delete_First (Input);
               Node := new WACC.AST.Statement_Node'(Typ => WACC.AST.N_Return, Exp => null);
               Parse_Exp (Node.Exp);
               Expect (WACC.Lexer.T_Semicolon);
            when WACC.Lexer.T_Semicolon =>
               Node := new WACC.AST.Statement_Node'(Typ => WACC.AST.N_Null);
               Expect (WACC.Lexer.T_Semicolon);
            when WACC.Lexer.T_if =>
               Delete_First (Input);
               Expect (WACC.Lexer.T_Open_Paren);
               Node := new WACC.AST.Statement_Node'(Typ => WACC.AST.N_If, others => <>);
               Parse_Exp (Node.Condition);
               Expect (WACC.Lexer.T_Close_Paren);
               Parse_Statement (Node.If_True);
               if Next_Token.Typ = WACC.Lexer.T_else then
                  Delete_First (Input);
                  Parse_Statement (Node.If_False);
               else
                  Node.If_False := null;
               end if;
            when WACC.Lexer.T_break =>
               Delete_First (Input);
               Node := new WACC.AST.Statement_Node'
                  (Typ   => WACC.AST.N_Break,
                   Label => Ada.Strings.Unbounded.Null_Unbounded_String);
               Expect (WACC.Lexer.T_Semicolon);
            when WACC.Lexer.T_continue =>
               Delete_First (Input);
               Node := new WACC.AST.Statement_Node'
                  (Typ   => WACC.AST.N_Continue,
                   Label => Ada.Strings.Unbounded.Null_Unbounded_String);
               Expect (WACC.Lexer.T_Semicolon);
            when WACC.Lexer.T_while =>
               Delete_First (Input);
               Expect (WACC.Lexer.T_Open_Paren);
               Node := new WACC.AST.Statement_Node'
                  (Typ => WACC.AST.N_While,
                   While_Label => WACC.Strings.Null_Identifier,
                   While_Condition => null,
                   While_Body => null);
               Parse_Exp (Node.While_Condition);
               Expect (WACC.Lexer.T_Close_Paren);
               Parse_Statement (Node.While_Body);
            when WACC.Lexer.T_do =>
               Delete_First (Input);
               Node := new WACC.AST.Statement_Node'
                  (Typ => WACC.AST.N_DoWhile,
                   While_Label => WACC.Strings.Null_Identifier,
                   While_Condition => null,
                   While_Body => null);
               Parse_Statement (Node.While_Body);
               Expect (WACC.Lexer.T_while);
               Expect (WACC.Lexer.T_Open_Paren);
               Parse_Exp (Node.While_Condition);
               Expect (WACC.Lexer.T_Close_Paren);
               Expect (WACC.Lexer.T_Semicolon);
            when WACC.Lexer.T_for =>
               Parse_For (Node);
            when WACC.Lexer.T_Open_Brace =>
               Node := new WACC.AST.Statement_Node'(Typ => WACC.AST.N_Compound, Block => null);
               Parse_Block (Node.Block);
            when others =>
               Node := new WACC.AST.Statement_Node'(Typ => WACC.AST.N_Expression, Exp => null);
               Parse_Exp (Node.Exp);
               Expect (WACC.Lexer.T_Semicolon);
         end case;
      end Parse_Statement;

      procedure Parse_Specifiers
         (Storage_Class : out WACC.AST.Any_Storage_Class_Node)
      is
         use type WACC.AST.Any_Storage_Class_Node;
         Int_Count : Natural := 0;
      begin
         Storage_Class := null;
         loop
            case Next_Token.Typ is
               when WACC.Lexer.T_extern =>
                  if Storage_Class /= null then
                     raise Parse_Error with "Only one storage class specifier is allowed";
                  else
                     Storage_Class := new WACC.AST.Storage_Class_Node'(Typ => WACC.AST.Extern);
                  end if;
               when WACC.Lexer.T_static =>
                  if Storage_Class /= null then
                     raise Parse_Error with "Only one storage class specifier is allowed";
                  else
                     Storage_Class := new WACC.AST.Storage_Class_Node'(Typ => WACC.AST.Static);
                  end if;
               when WACC.Lexer.T_int =>
                  Int_Count := Int_Count + 1;
               when others =>
                  exit;
            end case;
            Delete_First (Input);
         end loop;

         if Int_Count /= 1 then
            raise Parse_Error with "Expected int type";
         end if;
      end Parse_Specifiers;

      procedure Parse_Variable_Declaration
         (Node : out WACC.AST.Any_Variable_Declaration_Node)
      is
      begin
         if Next_Token.Typ /= WACC.Lexer.T_Identifier then
            raise Parse_Error with "Expected identifier after specifiers in variable declaration";
         end if;
         Node := new WACC.AST.Variable_Declaration_Node'
            (Name => Next_Token.Literal,
             Init => null,
             Storage_Class => null);
         Delete_First (Input);

         if Next_Token.Typ = WACC.Lexer.T_Equal then
            Delete_First (Input);
            Parse_Exp (Node.Init);
         end if;
         Expect (WACC.Lexer.T_Semicolon);
      end Parse_Variable_Declaration;

      procedure Parse_Param_List
         (Node : in out WACC.AST.Identifier_Vectors.Vector)
      is
         use WACC.AST.Identifier_Vectors;
      begin
         if Next_Token.Typ = WACC.Lexer.T_void then
            Delete_First (Input);
            Clear (Node);
            return;
         end if;

         loop
            Expect (WACC.Lexer.T_int);
            if Next_Token.Typ /= WACC.Lexer.T_Identifier then
               raise Parse_Error with "Expected Identifier after 'int' in param-list";
            end if;
            Append (Node, Next_Token.Literal);
            Delete_First (Input);
            exit when Next_Token.Typ /= WACC.Lexer.T_Comma;
            Delete_First (Input);
         end loop;
      end Parse_Param_List;

      procedure Parse_Function_Declaration
         (Node : out WACC.AST.Any_Function_Declaration_Node)
      is
      begin
         if Next_Token.Typ /= WACC.Lexer.T_Identifier then
            raise Parse_Error with "Expected identifier after specifiers in function declaration";
         end if;
         Node := new WACC.AST.Function_Declaration_Node'
            (Name => Next_Token.Literal,
             Params => WACC.AST.Identifier_Vectors.Empty_Vector,
             FBody => null,
             Storage_Class => null);
         Delete_First (Input);
         Expect (WACC.Lexer.T_Open_Paren);
         Parse_Param_List (Node.Params);
         Expect (WACC.Lexer.T_Close_Paren);
         if Next_Token.Typ = WACC.Lexer.T_Semicolon then
            Delete_First (Input);
         else
            Parse_Block (Node.FBody);
         end if;
      end Parse_Function_Declaration;

      procedure Parse_Declaration
         (Node : out WACC.AST.Any_Declaration_Node)
      is
         --  Lookahead past the type and identifier to figure out if this is a
         --  variable or function declaration.
         Storage_Class : WACC.AST.Any_Storage_Class_Node;
      begin
         Parse_Specifiers (Storage_Class);
         if Peek_Token (1).Typ = WACC.Lexer.T_Open_Paren then
            Node := new WACC.AST.Declaration_Node'
               (Typ => WACC.AST.N_FunDecl,
                Function_Declaration => null);
            Parse_Function_Declaration (Node.Function_Declaration);
            Node.Function_Declaration.Storage_Class := Storage_Class;
         else
            Node := new WACC.AST.Declaration_Node'
               (Typ => WACC.AST.N_VarDecl,
                Variable_Declaration => null);
            Parse_Variable_Declaration (Node.Variable_Declaration);
            Node.Variable_Declaration.Storage_Class := Storage_Class;
         end if;
      end Parse_Declaration;

      procedure Parse_Block_Item
         (Node : out WACC.AST.Any_Block_Item_Node)
      is
      begin
         case Next_Token.Typ is
            when WACC.Lexer.T_int | WACC.Lexer.T_static | WACC.Lexer.T_extern =>
               Node := new WACC.AST.Block_Item_Node'(Typ => WACC.AST.N_Declaration, others => <>);
               Parse_Declaration (Node.Decl);
            when others =>
               Node := new WACC.AST.Block_Item_Node'(Typ => WACC.AST.N_Statement, others => <>);
               Parse_Statement (Node.Stmt);
         end case;
      end Parse_Block_Item;

      procedure Parse_Block
         (Node : out WACC.AST.Any_Block_Node)
      is
         use type WACC.AST.Any_Block_Item_Node;
         Tail, Next : WACC.AST.Any_Block_Item_Node := null;
      begin
         Expect (WACC.Lexer.T_Open_Brace);
         Node := new WACC.AST.Block_Node'(Head => null);

         while Next_Token.Typ /= WACC.Lexer.T_Close_Brace loop
            Parse_Block_Item (Next);
            if Tail = null then
               Node.Head := Next;
            else
               Tail.Next := Next;
            end if;
            Tail := Next;
         end loop;
         Expect (WACC.Lexer.T_Close_Brace);
      end Parse_Block;

      Next_Decl : WACC.AST.Any_Declaration_Node;
   begin
      while not Is_Empty (Input) loop
         Parse_Declaration (Next_Decl);
         WACC.AST.Declaration_Vectors.Append (Tree.Declarations, Next_Decl);
      end loop;
   end Parse_Program;

end WACC.Parser;
