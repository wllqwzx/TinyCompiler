type basetype =
  | Int_ty 
  | Bool_ty
  | Unit_ty

type expression =
  | Const_exp     of int
  | Is_Zero_exp   of expression
  | Sub_exp       of expression * expression
  | Add_exp       of expression * expression
  | Mul_exp       of expression * expression
  | Div_exp       of expression * expression
  | Var_exp       of string
  | Call_exp      of expression * expression

type statements =
  | Empty_stat    
  | Combine_stat  of statements * statements   
  | Var_def_stat  of basetype * string * expression
  | Var_set_stat  of string * expression 
  | If_stat       of statements * statements
  | While_stat    of expression * statements


type function_def = 
  | A_func of basetype * string * (string * basetype) list * statements * expression
  
type program =
  | A_program of function_def
