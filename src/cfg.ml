open Core.Std
open Lexing
open Array

open Parser
open Lexer
open Ast
open IrAst
open Util

let funcHtb = Hashtbl.create ~hashable:String.hashable ()
let edgeHtb = Hashtbl.create ~hashable:String.hashable ()


let transCommarrToHtb =
    fun name commarr ->
    let mat = Array.make_matrix 50 50 0 in    (* a matrix *)
    let nodesHtb = Hashtbl.create ~hashable:Core_kernel.Std_kernel.Int.hashable () in 
    let curLab = ref 0 in
    let processComm =
        fun ind comm -> 
        match comm with 
        | Ir_label  labid -> let _ = Hashtbl.add nodesHtb labid (ref [||]) in 
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
    Array.iteri processComm commarr;
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



let test = 
    fun x ->
    print_int x

