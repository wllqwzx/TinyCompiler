open Core.Std
open Lexing

open Parser
open Lexer
open Ast
open IrAst

let labelid = ref 0

let getLabel () =
    labelid := !labelid + 1;
    !labelid

let tempVarId = ref 0

let getNewVarId () =
    tempVarId := !tempVarId + 1;
    !tempVarId




let rec irgen_exp = 
    fun exp -> 
    match exp with
    | Const_bool_exp bol ->     let tvarName = "_t" ^ (string_of_int (getNewVarId ())) in
                               if bol then [|(Ir_assign (tvarName, (Ir_constant 1)))|] 
                                      else [|(Ir_assign (tvarName, (Ir_constant 0)))|]
    | Lt_exp (exp1, exp2) ->    let tt1 = irgen_exp exp1 in
                               let tvar1 = Ir_var ("_t" ^ (string_of_int !tempVarId)) in 
                               let tt2 = irgen_exp exp2 in
                               let tvar2 = Ir_var ("_t" ^ (string_of_int !tempVarId)) in
                               let tt3 = Ir_biop (tvar1, Lt, tvar2) in
                               Array.append (Array.append tt1 tt2) tt3
    | And_exp (exp1, exp2) ->   let tt1 = irgen_exp exp1 in
                               let tvar1 = Ir_var ("_t" ^ (string_of_int !tempVarId)) in 
                               let tt2 = irgen_exp exp2 in
                               let tvar2 = Ir_var ("_t" ^ (string_of_int !tempVarId)) in
                               let tt3 = Ir_biop (tvar1, And, tvar2) in
                               Array.append (Array.append tt1 tt2) tt3
    | Or_exp (exp1, exp2) ->    let tt1 = irgen_exp exp1 in
                               let tvar1 = Ir_var ("_t" ^ (string_of_int !tempVarId)) in 
                               let tt2 = irgen_exp exp2 in
                               let tvar2 = Ir_var ("_t" ^ (string_of_int !tempVarId)) in
                               let tt3 = Ir_biop (tvar1, Or, tvar2) in
                               Array.append (Array.append tt1 tt2) tt3
    | Const_int_exp num ->      let tvarName = "_t" ^ (string_of_int (getNewVarId ())) in
                               [|(Ir_assign (tvarName, (Ir_constant num)))|]  
    | Sub_exp (exp1, exp2) ->    let tt1 = irgen_exp exp1 in
                               let tvar1 = Ir_var ("_t" ^ (string_of_int !tempVarId)) in 
                               let tt2 = irgen_exp exp2 in
                               let tvar2 = Ir_var ("_t" ^ (string_of_int !tempVarId)) in
                               let tt3 = Ir_biop (tvar1, Sub, tvar2) in
                               Array.append (Array.append tt1 tt2) tt3
    | Add_exp (exp1, exp2) ->   let tt1 = irgen_exp exp1 in
                               let tvar1 = Ir_var ("_t" ^ (string_of_int !tempVarId)) in 
                               let tt2 = irgen_exp exp2 in
                               let tvar2 = Ir_var ("_t" ^ (string_of_int !tempVarId)) in
                               let tt3 = Ir_biop (tvar1, Sub, tvar2) in
                               Array.append (Array.append tt1 tt2) tt3
    | Mul_exp (exp1, exp2) ->   let tt1 = irgen_exp exp1 in
                               let tvar1 = Ir_var ("_t" ^ (string_of_int !tempVarId)) in 
                               let tt2 = irgen_exp exp2 in
                               let tvar2 = Ir_var ("_t" ^ (string_of_int !tempVarId)) in
                               let tt3 = Ir_biop (tvar1, Mul, tvar2) in
                               Array.append (Array.append tt1 tt2) tt3
    | Div_exp (exp1, exp2) ->  let tt1 = irgen_exp exp1 in
                               let tvar1 = Ir_var ("_t" ^ (string_of_int !tempVarId)) in 
                               let tt2 = irgen_exp exp2 in
                               let tvar2 = Ir_var ("_t" ^ (string_of_int !tempVarId)) in
                               let tt3 = Ir_biop (tvar1, Div, tvar2) in
                               Array.append (Array.append tt1 tt2) tt3
    | Var_exp str ->            let tvarName = "_t" ^ (string_of_int (getNewVarId ())) in
                               [|(Ir_assign (tvarName, (Ir_var str)))|]
    | Call_exp (str, expli) -> let tt1 = irgen_call_params expli [||] in
                              let tt2 = [|(Ir_call str)|] in
                              Array.append tt1 tt2

and irgen_call_params =
    fun expli tvarArray -> 
    match expli with
    | [] -> let res = Array.make (Array.length tvarArray) (Ir_push (Ir_var "NONE")) in
           for t =0 to (Array.length tvarArray) - 1 do
               Array.set res t [|(Ir_push (Array.get tvarArray t))|]
           done;
           res 
    | e::el -> let tt1 = irgen_exp e in
              let tvar = Ir_var ("_t" ^ (string_of_int !tempVarId)) in
              let tt2 = irgen_call_params el (Array.append tvarArray [|tvar|]) in
              Array.append tt1 tt2

let rec irgen_commli =
    fun commli arr -> 
    match commli with
    | [] -> arr   
    | (Var_def_stat (ty, str, exp))::cli   ->  let tt = Array.append arr (irgen_exp exp) in
                                              let tvar = Ir_var ("_t" ^ (string_of_int !tempVarId)) in
                                              let tt2 = Array.append tt [|(Ir_assign (str, tvar))|] in 
                                              irgen_commli cli tt2
    | (Var_set_stat (str, exp))::cli       ->  let tt = Array.append arr (irgen_exp exp) in
                                              let tvar = Ir_var ("_t" ^ (string_of_int !tempVarId)) in
                                              let tt2 = Array.append tt [|(Ir_assign (str, tvar))|] in 
                                              irgen_commli cli tt2
    | (If_stat (exp, commli1, commli2))::cli -> let tt = Array.append arr (irgen_exp exp) in
                                               let tvar = Ir_var ("_t" ^ (string_of_int !tempVarId)) in
                                               let lb1 = getLabel () in
                                               let lb2 = getLabel () in 
                                               let lb3 = getLabel () in
                                               let tt2 = Array.append tt [|(Ir_ifz (tvar, lb2))|] in
                                               let tt3 = Array.append tt2 [|(Ir_goto lb1)|] in 
                                               let tt4 = Array.append tt3 [|(Ir_label lb1)|] in 
                                               let tt5 = Array.append tt4 (irgen_commli commli1 [||]) in
                                               let tt6 = Array.append tt5 [|(Ir_goto lb3)|] in
                                               let tt7 = Array.append tt6 [|(Ir_label lb2)|] in
                                               let tt8 = Array.append tt7 (irgen_commli commli2 [||]) in
                                               let tt9 = Array.append tt8 [|(Ir_goto lb3)|] in
                                               let tt10 = Array.append tt9 [|(Ir_label lb3)|] in
                                               irgen_commli cli tt10
    | (While_stat   (exp, commli1))::cli   ->   let tt = Array.append arr (irgen_exp exp) in
                                               let tvar = Ir_var ("_t" ^ (string_of_int !tempVarId)) in
                                               let lb1 = getLabel () in
                                               let lb2 = getLabel () in 
                                               let tt2 = Array.append tt [|(Ir_ifz (tvar, lb2))|] in
                                               let tt3 = Array.append tt2 [|(Ir_goto lb1)|] in 
                                               let tt4 = Array.append tt3 [|(Ir_label lb1)|] in 
                                               let tt5 = Array.append tt4 (irgen_commli commli1 [||]) in
                                               let tt6 = Array.append tt5 (irgen_exp exp) in
                                               let tvar = Ir_var ("_t" ^ (string_of_int !tempVarId)) in
                                               let tt7 = Array.append tt6 [|(Ir_ifz (tvar, lb2))|] in
                                               let tt8 = Array.append tt7 [|(Ir_goto lb1)|] in
                                               let tt9 = Array.append tt8 [|(Ir_label lb2)|] in
                                               irgen_commli cli tt9
    | (Print_stat   exp)::cli              ->   let tt = Array.append arr (irgen_exp exp) in
                                               let tvar = Ir_var ("_t" ^ (string_of_int !tempVarId)) in
                                               let tt1 = Array.append tt [|(Ir_print tvar)|] in
                                               irgen_commli cli tt1
    | (Return_stat  exp)::cli              ->   let tt = Array.append arr (irgen_exp exp) in
                                               let tvar = Ir_var ("_t" ^ (string_of_int !tempVarId)) in
                                               let tt1 = Array.append tt [|(Ir_ret tvar)|] in
                                               irgen_commli cli tt1




let rec irgen_param =
    fun paramli ->
    match paramli with
    | [] -> []
    | A_param (str, ty)::pl -> str::(irgen_param pl) 

let rec irgen_pop_param = (* [|It_pop ...|] *)
    fun paramli arr ->
    match paramli with
    | [] -> arr
    | s::sl -> irgen_pop_param sl (Array.append (Array.append arr [|(Ir_pop s)|]))

let irgen_func = 
    fun func ->
    match func with
    | A_func (ty, name, paramli, commli) -> Ir_a_function (name, (irgen_param paramli), (Array.append (Array.append [|(IrAst.Ir_label (getLabel ()))|] (irgen_pop_param paramli [||])) (irgen_commli commli [||])))

let rec irgen_func_list = 
    fun func_list ->
    match func_list with
    | [] -> [] 
    | f::fl -> (irgen_func f)::(irgen_func_list fl)


let irgen = 
    fun pgm ->
    match pgm with
    | A_program func_list -> Ir_a_program (irgen_func_list func_list)
