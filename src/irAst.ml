type op = 
    | Add
    | Sub
    | Mul
    | Div 
    | And 
    | Or     

type label = string

type irvar = string

type funName = string

type biexp = 
    | Ir_constant  of int 
    | Ir_irvar     of irvar
    | Ir_vov       of irvar * op * irvar
    | Ir_cov       of int * op * irvar
    | Ir_voc       of irvar * op * int
    | Ir_coc       of int * op * int
    | Ir_call      of funName

type command =
    | Ir_assign of irvar * biexp
    | Ir_goto   of label
    | Ir_ifz    of irvar * command

type basicblock = 
    | Ir_a_block of label * command list

type ir_function_def = 
    | Ir_a_function of funName * irvar list * basicblock list

type ir_program = 
    | Ir_a_program of ir_function_def list 