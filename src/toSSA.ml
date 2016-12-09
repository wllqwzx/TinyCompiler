open Core.Std
open Lexing
open Array

open Parser
open Lexer
open Ast
open IrAst
open Util
open Cfg



let transFuncToSSA = 
    fun key data ->
    let mat = Hashtbl.find Cfg.edgeHtb key in
    
    match mat with
    | Some edgeMat -> 
    | None -> print_string "impossible!"   

let transToSSA = 
    fun () ->
    Hashtbl.iteri Cfg.funcHtb transFuncToSSA