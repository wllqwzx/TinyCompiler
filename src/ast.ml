type basetype =
  | Int_ty 
  | Bool_ty
  | Unit_ty

type expression =
  | Const_bool_exp  of bool
  | Lt_exp          of expression * expression
  | And_exp         of expression * expression
  | Or_exp          of expression * expression
  | Const_int_exp   of int
  | Sub_exp         of expression * expression
  | Add_exp         of expression * expression
  | Mul_exp         of expression * expression
  | Div_exp         of expression * expression
  | Var_exp         of string
  | Call_exp        of string * expression list

type statement =
  | Empty_stat
  | Exp_stat      of expression  
  | Var_def_stat  of basetype * string * expression
  | Var_set_stat  of string * expression 
  | If_stat       of expression * statement list * statement list
  | While_stat    of expression * statement list
  | Print_stat    of expression

type param =
  | A_param of string * basetype

type function_def = 
  | A_func of basetype * string * param list * statement list

type program =
  | A_program of function_def list
