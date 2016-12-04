open Core.Std
open Lexing
open Array

open Parser
open Lexer
open Ast
open IrAst

let funcHtb = Hashtbl.create ~hashable:String.hashable ()



let transCommarrToHtb =
    fun commarr ->
    


let makeFuncCFG = 
    fun fundef ->
    match fundef with
    | Ir_a_function (name, params, commarr) -> Hashtbl.add funcHtb name (transCommarrToHtb commarr) 

let rec makeFuncliCFG = 
    fun fundefli ->
    match fundefli with
    | [] -> ()
    | f::fl -> makeFuncCFG f; makeFuncliCFG fl 

let makeCFG =
    fun irpgm -> 
    match irpgm with
    | Ir_a_program fun_def_list -> makeFuncliCFG fun_def_list


