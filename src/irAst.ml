type op = 
    | Add
    | Sub
    | Mul
    | Div 
    | And 
    | Or     

type irexp = 
    | Ir_constant  of int 
    | Ir_var       of string
    | Ir_vov       of string * op * string
    | Ir_cov       of int * op * string
    | Ir_voc       of string * op * int
    | Ir_coc       of int * op * int
    | Ir_call      of string (* funName *)

type command =
    | Ir_label  of int
    | Ir_assign of string * irexp
    | Ir_goto   of int  (* label *)
    | Ir_ifz    of string * int (* var * label *)
    | Ir_push   of string
    | Ir_pop    of string
    | Ir_ret    of irexp
(*
type basicblock =   (* label *)
    | Ir_a_block of string * command list
*)

type ir_function_def = (* funName * var list * ... *)
    | Ir_a_function of string * string list * command array

type ir_program = 
    | Ir_a_program of ir_function_def list 