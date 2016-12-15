type op = 
    | Add
    | Sub
    | Mul
    | Div 
    | And 
    | Or     
    | Lt 


type irexp = 
    | Ir_constant  of int 
    | Ir_var       of string
    | Ir_biop      of irexp * op * irexp (* only: constant or var *)
    | Ir_call      of string (* funName *)
    | Ir_Phi       of irexp * irexp

type command = 
    | Ir_label  of int
    | Ir_assign of string * irexp
    | Ir_goto   of int  (* label *)
    | Ir_ifz    of irexp * int (* only: constant or var *) (* label *)
    | Ir_push   of irexp (* only: constant or var *)
    | Ir_pop    of string
    | Ir_print  of irexp (* only: constant or var *)
    | Ir_ret    of irexp (* only: constant or var *)

(*
type basicblock =   (* label *)
    | Ir_a_block of string * command list
*)

type ir_function_def = (* funName * var list * ... *)
    | Ir_a_function of string * string list * command array

type ir_program = 
    | Ir_a_program of ir_function_def list 