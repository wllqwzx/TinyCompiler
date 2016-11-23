type ir_param = 
    | Ir_a_param of string

type op = 
    | Add
    | Sub
    | Mul
    | Div 
    | And 
    | Or     

type biexp = 
    | Ir_constant  of int 
    | Ir_var       of string
    | Ir_vov       of string * op * string
    | Ir_cov       of int * op * string
    | Ir_voc       of string * op * int
    | Ir_coc       of int * op * int
    | Ir_call      of string * ir_param list 

type command =
    | Ir_assign of string * biexp
    | Ir_if     of string * command

type basicblock = 
    | Ir_a_block of string * command list

type ir_function_def = 
    | Ir_a_function of string * ir_param list * basicblock list

type ir_program = 
    | Ir_a_program of ir_function_def list 