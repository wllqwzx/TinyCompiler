open Core.Std
open Lexing
open Array

open Parser
open Lexer
open Ast
open IrAst
open Util
open Cfg

(* 1 *)
let outPutEdge =
    fun edgeMat ->
    let fout = Out_channel.create ".temp.in" in
    for i = 0 to 99 do
        for j = 0 to 99 do
            if phys_equal edgeMat.(i).(j) 1 then
                fprintf fout "%d %d\n" i j
            else
                ()
        done
    done;
    Out_channel.close fout



let addLineToDomFtr = 
    fun domFtr ->
    fun str ->
    let strList = String.split str ' ' in
    let intList = List.map strList (fun str -> int_of_string str) in
    let Some bid = List.hd intList in
    let Some tail = List.tl intList in
    let Some count = List.hd tail in 
    let nodesOpt = List.tl tail in
    let arr = ref [||] in
    let rec transToArr =
        fun nds -> 
        match nds with
        | [] -> [||]
        | n::nl -> Array.append [|n|] (transToArr nl)
    in
    match nodesOpt with
    | None -> Hashtbl.add domFtr bid arr; () 
    | Some nodes -> let narr = transToArr nodes in
                   arr := narr;
                   Hashtbl.add domFtr bid arr; ()

(* 2 *)
let getDomFrontier =
    fun () ->
    let domFtr = Hashtbl.create ~hashable:Core_kernel.Std_kernel.Int.hashable () in
    Sys.command "a.out";
    let file = In_channel.create ".temp.out" in
    let strings = In_channel.input_lines file in
    let _ = List.map strings (addLineToDomFtr domFtr) in 
    In_channel.close file;
    Sys.command "rm .temp.in";
    Sys.command "rm .temp.out";
    domFtr


(* 3 *)
let addPhiFunc = 
    fun nodeHtb domFrt ->
    

(* 4 *)
let reName =
    fun nodeHtb ->



let transFuncToSSA = 
    fun key data ->
    let mat = Hashtbl.find Cfg.edgeHtb key in
    match mat with
    | Some edgeMat -> outPutEdge edgeMat;
                     let domFrt = getDomFrontier () in
                     addPhiFunc data domFrt;
                     reName data
    | None -> print_string "impossible!"   


let transToSSA = 
    fun () ->
    Hashtbl.iteri Cfg.funcHtb transFuncToSSA