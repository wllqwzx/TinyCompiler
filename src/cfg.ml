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
let fatherArray = Hashtbl.create ~hashable:String.hashable () (* funcName ---> hashTable:(Lid ---> ref arr) *)

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

(* -------- makeFatherArray ------- *)

let makeFatherArray = (* call after makeCFG *) 
    fun () -> 
    let makeFatherArrayInFunc = 
        fun ~key ~data ->
        let fatherArrayInFunc = Hashtbl.create ~hashable:Core_kernel.Std_kernel.Int.hashable () in
        for i = 0 to 99 do
            for j = 0 to 99 do
                let t = data.(i).(j) in
                if t = 1 then
                    let has_i = Hashtbl.find fatherArrayInFunc i in
                    let has_j = Hashtbl.find fatherArrayInFunc j in
                    begin
                    match has_i with
                    | None -> Hashtbl.add fatherArrayInFunc i (ref [||]);()
                    | Some arrref -> ()
                    end;
                    begin 
                    match has_j with
                    | None -> Hashtbl.add fatherArrayInFunc j (ref [|i|]); ()
                    | Some arrref -> Util.insertBack arrref i
                    end
                else ()
            done
        done;
        Hashtbl.add fatherArray key fatherArrayInFunc;()
    in
    Hashtbl.iteri edgeHtb makeFatherArrayInFunc 



(* ------- print cfg ------- *)
let print_op =
    fun op file ->
    match op with
    | Add -> fprintf file " + "
    | Sub -> fprintf file " - "
    | Mul -> fprintf file " * "
    | Div -> fprintf file " / "
    | And -> fprintf file " && "
    | Or  -> fprintf file " || "    
    | Lt  -> fprintf file " < "

let rec print_irexp =
    fun irexp file ->
    match irexp with
    | Ir_constant num       -> fprintf file "%d" num 
    | Ir_var str            -> fprintf file "%s" str
    | Ir_biop (e1, op, e2)  -> print_irexp e1 file; print_op op file; print_irexp e2 file
    | Ir_call name          -> fprintf file "%s" ("call " ^ name)
    | Ir_Phi (e1, e2)       -> fprintf file "%s" "Phi("; print_irexp e1 file; fprintf file "%s" ", "; print_irexp e2 file; fprintf file ")"


let print_Id_Block =
    fun bid commarrRef file ->
    fprintf file "%s" ("\n _B" ^ (string_of_int bid) ^ ":\n");
    for i = 0 to (Array.length !commarrRef) - 1 do
        let comm = !commarrRef.(i) in
        match comm with
        | Ir_label num           -> fprintf file "%s" ("_B" ^ (string_of_int num) ^ ":"); fprintf file "\n"
        | Ir_assign (str, irexp) -> fprintf file "%s" (str ^ " = "); print_irexp irexp file; fprintf file "\n"
        | Ir_goto num            -> fprintf file "%s" ("goto _B" ^ (string_of_int num)); fprintf file "\n"
        | Ir_ifz (irexp, num)    -> fprintf file "%s" "ifz "; print_irexp irexp file; fprintf file "%s" (" goto _B" ^ (string_of_int num) ^ ":"); fprintf file "\n"
        | Ir_push irexp          -> fprintf file "%s" "push "; print_irexp irexp file; fprintf file "\n"
        | Ir_pop str             -> fprintf file "%s" ("pop " ^ str); fprintf file "\n"
        | Ir_print irexp         -> fprintf file "%s" "print "; print_irexp irexp file; fprintf file "\n"
        | Ir_ret irexp           -> fprintf file "%s" "ret "; print_irexp irexp file; fprintf file "\n"
    done

let print_func_seq = 
    fun file -> 
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
                       | Some commarrRef -> print_Id_Block bid commarrRef file
                       end   
                   done 


let print_CFG = 
    fun fileName -> 
    let file = Out_channel.create fileName in
    Hashtbl.iteri nodeSeq (print_func_seq file);
    Out_channel.close file
