type expression =
  | Const_exp     of int
  | Diff_exp      of expression * expression
  | Is_zero_exp   of expression
  | If_exp        of expression * expression * expression
  | Var_exp       of string
  | Let_exp       of string * expression * expression
  | Proc_exp      of string * expression
  | Call_exp      of expression * expression
  | Begin_exp     of expression list
  | Newref_exp    of expression
  | Deref_exp     of expression
  | Setref_exp    of expression * expression

type program =
  | A_program of expression
