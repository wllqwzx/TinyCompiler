open Core.Std
open Lexing
open Array

open Parser
open Lexer
open Ast
open IrAst
open Util

let funcHtb = Hashtbl.create ~hashable:String.hashable ()   (* funName ---> hashTable:(Lid ---> ref arr) *)
let edgeHtb = Hashtbl.create ~hashable:String.hashable ()   (* funName ---> matrix *)
let nodeSeq = Hashtbl.create ~hashable:String.hashable ()   (* funName ---> int array *)

let transCommarrToHtb =
    fun name commarr ->
    let mat = Array.make_matrix 100 100 0 in    (* a edge matrix *)
    let nodesHtb = Hashtbl.create ~hashable:Core_kernel.Std_kernel.Int.hashable () in  (* basic block hash table *)
    let seq = ref [||] in
    let curLab = ref 0 in
    let processComm =
        fun ind comm -> 
        match comm with 
        | Ir_label  labid -> let _ = Hashtbl.add nodesHtb labid (ref [||]) in 
                            Util.insertBack seq labid;
                            curLab := labid
        | Ir_goto   labid -> mat.(!curLab).(labid) <- 1;
                            let tt = Hashtbl.find nodesHtb !curLab in
                            begin
                            match tt with
                            | Some refarr -> Util.insertBack refarr (Ir_goto labid)
                            | None -> print_string "impossible!"
                            end
        | Ir_ifz    (irexp, labid) -> mat.(!curLab).(labid) <- 1;
                                     let tt = Hashtbl.find nodesHtb !curLab in
                                     begin
                                     match tt with
                                     | Some refarr -> Util.insertBack refarr (Ir_ifz (irexp, labid))
                                     | None -> print_string "impossible!"
                                     end
        | othercomm -> let tt = Hashtbl.find nodesHtb !curLab in
                      begin
                      match tt with
                      | Some refarr -> Util.insertBack refarr othercomm
                      | None -> print_string "impossible!"
                      end
    in
    Array.iteri processComm commarr;   (* trans comm arr to edge matrix and basic block hash table *)
    let _ = Hashtbl.add nodeSeq name seq in
    let _ = Hashtbl.add edgeHtb name mat in
    nodesHtb


let makeFuncCFG = 
    fun fundef ->
    match fundef with
    | Ir_a_function (name, params, commarr) -> let _ = Hashtbl.add funcHtb name (transCommarrToHtb name commarr) in ()

let rec makeFuncliCFG = 
    fun fundefli ->
    match fundefli with
    | [] -> ()
    | f::fl -> makeFuncCFG f; makeFuncliCFG fl 

let makeCFG =
    fun irpgm -> 
    match irpgm with
    | Ir_a_program fun_def_list -> makeFuncliCFG fun_def_list


(* ------- print cfg ------- *)
let print_op =
    fun op ->
    match op with
    | Add -> print_string " + "
    | Sub -> print_string " - "
    | Mul -> print_string " * "
    | Div -> print_string " / "
    | And -> print_string " && "
    | Or  -> print_string " || "    
    | Lt  -> print_string " < "

let rec print_irexp =
    fun irexp ->
    match irexp with
    | Ir_constant num       -> print_int num 
    | Ir_var str            -> print_string str
    | Ir_biop (e1, op, e2)  -> print_irexp e1; print_op op; print_irexp e2
    | Ir_call name          -> print_string ("call " ^ name)
    | Ir_Phi (str1, str2)   -> print_string ("Phi(" ^ str1 ^ ", " ^ str2 ^ ")")


let print_Id_Block =
    fun bid commarrRef ->
    print_string ("\n _B" ^ (string_of_int bid) ^ ":\n");
    for i = 0 to (Array.length !commarrRef) - 1 do
        let comm = !commarrRef.(i) in
        match comm with
        | Ir_label num           -> print_string ("_B" ^ (string_of_int num) ^ ":"); print_newline ()
        | Ir_assign (str, irexp) -> print_string (str ^ " = "); print_irexp irexp; print_newline ()
        | Ir_goto num            -> print_string ("goto _B" ^ (string_of_int num)); print_newline ()
        | Ir_ifz (irexp, num)    -> print_string "ifz "; print_irexp irexp; print_string (" goto _B" ^ (string_of_int num) ^ ":"); print_newline ()  
        | Ir_push irexp          -> print_string "push "; print_irexp irexp; print_newline ()
        | Ir_pop str             -> print_string ("pop " ^ str); print_newline ()
        | Ir_print irexp         -> print_string "print "; print_irexp irexp; print_newline ()
        | Ir_ret irexp           -> print_string "ret "; print_irexp irexp; print_newline ()
    done

let print_func_seq = 
    fun ~key ~data ->
    let baseBOpt = Hashtbl.find funcHtb key in
    match baseBOpt with
    | None -> print_string "impossible!"
    | Some baseB -> for i = 0 to (Array.length !data) - 1 do
                       let bid = !data.(i) in
                       let commarrOpt = Hashtbl.find baseB bid in
                       begin
                       match commarrOpt with
                       | None -> print_string "impossible!"
                       | Some commarrRef -> print_Id_Block bid commarrRef
                       end   
                   done 


let print_CFG = 
    fun () -> 
    Hashtbl.iteri nodeSeq print_func_seq
