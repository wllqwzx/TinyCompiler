%token L_PAREN
%token R_PAREN
%token MINUS
%token EQUAL
%token COMMA
%token SEMICOLEN

%token LET
%token IN
%token IF
%token THEN
%token ELSE
%token IS_ZERO
%token PROC
%token BEGIN
%token END
%token NEWREF
%token DEREF
%token SETREF

%token <int>NUM
%token <string>ID

%token EOF

%start <Ast.program> enterPoint
%%

enterPoint:
    | ast = nt_program; EOF         { ast }

nt_program:
    | ast = nt_exp                  { Ast.A_program ast }

nt_exp:
    | ast = NUM                     { Ast.Const_exp ast }
    | MINUS; L_PAREN; ast1 = nt_exp; COMMA; ast2 = nt_exp; R_PAREN
                                    { Ast.Diff_exp (ast1, ast2) }
    | IS_ZERO; L_PAREN; ast = nt_exp; R_PAREN
                                    { Ast.Is_zero_exp ast }
    | IF; ast1 = nt_exp; THEN; ast2 = nt_exp; ELSE; ast3 = nt_exp
                                    { Ast.If_exp (ast1,ast2,ast3) }
    | ast = ID                      { Ast.Var_exp ast }
    | LET; ast1 = ID; EQUAL; ast2 = nt_exp; IN; ast3 = nt_exp
                                    { Ast.Let_exp (ast1, ast2, ast3) }
    | PROC; L_PAREN; ast1 = ID; R_PAREN; EQUAL; ast2 = nt_exp
                                    { Ast.Proc_exp (ast1, ast2) }
    | L_PAREN; ast1 = nt_exp; ast2 = nt_exp; R_PAREN
                                    { Ast.Call_exp (ast1, ast2) }
    | BEGIN; astlist = separated_list (SEMICOLEN, nt_exp); END
                                    { Ast.Begin_exp astlist }
    | NEWREF; L_PAREN; ast = nt_exp; R_PAREN
                                    { Ast.Newref_exp ast }
    | DEREF; L_PAREN; ast = nt_exp; R_PAREN
                                    { Ast.Deref_exp ast }
    | SETREF; L_PAREN; ast1 = nt_exp; COMMA; ast2 = nt_exp; R_PAREN
                                    { Ast.Setref_exp (ast1, ast2) }
