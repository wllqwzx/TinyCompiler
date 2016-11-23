open Core.Std
open Lexing

open Parser
open Lexer
open Ast
open IrAst


let rec irgen_basicblock = 
    fun statli ->
    | 

let rec irgen_param =
    fun paramli ->
    match paramli with
    | [] -> []
    | (str, ty)::pl -> (Ir_a_param str)::(irgen_param pl) 

let irgen_func = 
    fun func ->
    match func with
    | A_func (ty, name, paramli, statli) -> Ir_a_function (name, (irgen_param paramli), (irgen_basicblock statli))

let rec irgen_func_list = 
    fun func_list ->
    match func_list with
    | [] -> [] 
    | f::fl -> (irgen_func f)::(irgen_func_list fl)


let irgen = 
    fun pgm ->
    match pgm with
    | A_program func_list -> Ir_a_program (irgen_func_list func_list)
