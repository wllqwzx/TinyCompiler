%token L_PAREN    
%token R_PAREN
%token L_BIG_PAREN
%token R_BIG_PAREN
%token EQUAL
%token COMMA
%token SEMICOLEN
%token COLON


%token IF
%token ELSE
%token WHILE
%token PASS
%token AND
%token OR
%token LT
%token ADD
%token SUB
%token MUL
%token DIV
%token PRINT
%token TRUE
%token FALSE

%token INT_TY
%token BOOL_TY
%token VOID_TY

%token <int>NUM
%token <string>ID

%token EOF

%start <Ast.program> enterPoint
%%
enterPoint:
    | ast = nt_program; EOF         {ast}

nt_program:
    | astlist = separated_list(SEMICOLEN, nt_func_def)
                                    {Ast.A_program astlist}

nt_func_def:
    | ty = nt_type; ast1 = ID; L_PAREN; paramlist = separated_list(COMMA, nt_aparam) ;R_PAREN; L_BIG_PAREN; statlist = separated_list (SEMICOLEN, nt_stat); R_BIG_PAREN
                                    {Ast.A_func (ty, ast1, paramlist, statlist, )}

nt_type:
    | INT_TY                        {Ast.Int_ty}
    | BOOL_TY                       {Ast.Bool_ty}
    | VOID_TY                       {Ast.Unit_ty}

nt_aparam:
    | str = ID; COLON; ty = nt_type {Ast.A_param (str, ty)}

nt_stat:
    | PASS                          {Ast.Empty_stat}
    | ast = nt_exp                  {Ast.Exp_stat ast}
    | ty = nt_type; str = ID; EQUAL; ast = nt_exp
                                    {Ast.Var_def_stat (ty, str, ast)}
    | std = ID; EQUAL; ast = nt_exp {Ast.Var_set_stat (str, ast)}
    | IF; L_PAREN; ast1 = nt_exp; R_PAREN; L_BIG_PAREN; ast2 = separated_list(SEMICOLEN; nt_stat); R_BIG_PAREN; ELSE; L_BIG_PAREN; ast3 = separated_list(SEMICOLEN; nt_stat); R_BIG_PAREN
                                    {Ast.If_exp (ast1, ast2, ast3)}
    | WHILE; L_PAREN; ast1 = nt_exp; R_PAREN; L_BIG_PAREN; ast2 = separated_list(SEMICOLEN; nt_stat); R_BIG_PAREN
                                    {Ast.While_stat (ast1, ast2)}
    | PRINT; L_PAREN; ast = nt_exp; R_PAREN
                                    {Ast.Print_stat ast} 

nt_exp:
    | TRUE                          {Ast.Const_bool_exp true}
    | FALSE                         {Ast.Const_bool_exp false}
    | LT; L_PAREN; ast1 = nt_exp; COMMA; ast2 = nt_exp; R_PAREN
                                    {Ast.Lt_exp (ast1, ast2)}
    | AND; L_PAREN; ast1 = nt_exp; COMMA; ast2 = nt_exp; R_PAREN
                                    {Ast.Add_exp (ast1, ast2)}
    | OR; L_PAREN; ast1 = nt_exp; COMMA; ast2 = nt_exp; R_PAREN
                                    {Ast.Or_exp (ast1, ast2)}
    | num = NUM;                    {Ast.Const_int_exp num}
    | SUB; L_PAREN; ast1 = nt_exp; COMMA; ast2 = nt_exp; R_PAREN
                                    {Ast.Sub_exp (ast1, ast2)}
    | ADD; L_PAREN; ast1 = nt_exp; COMMA; ast2 = nt_exp; R_PAREN
                                    {Ast.Add_exp (ast1, ast2)}
    | MUL; L_PAREN; ast1 = nt_exp; COMMA; ast2 = nt_exp; R_PAREN
                                    {Ast.Mul_exp (ast1, ast2)}
    | DIV; L_PAREN; ast1 = nt_exp; COMMA; ast2 = nt_exp; R_PAREN
                                    {Ast.Div_exp (ast1, ast2)}
    | ast = ID                      {Ast.Var_exp ast}
    | ast1 = ID; L_PAREN; paramlist = separated_list(COMMA, nt_exp); R_PAREN
                                    {Ast.Call_exp (ast1, paramlist)} 
