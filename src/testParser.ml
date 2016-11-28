(* testParser.ml transform ast to easy read code and print out to test if the parser is correct *)

open Core.Std
open Lexing

open Parser
open Lexer
open Ast
open IrAst
open IrGen

let ast =
    match In_channel.input_line stdin with (* read from terminal, we modify it to read from a file *)
    | None -> print_endline "\nGood bye."; exit 0
    | Some line -> let alexbuf = Lexing.from_string line in
                   try
                   Parser.enterPoint Lexer.read alexbuf
                   with
                   | Lexer.LexerError str -> print_string str; exit 1
                   | Parser.Error -> print_string ("Oops!!! parser error with char: " ^ (Lexing.lexeme alexbuf)
                                                     ^ " at: " ^ (Lexer.error_info alexbuf)); exit 1

(* ---------- test parser ---------- *)

let print_type =
    fun ty ->
    match ty with
    | Int_ty    -> print_string "int "
    | Bool_ty   -> print_string "bool "
    | Unit_ty   -> print_string "void "

let rec print_exp = 
    fun exp ->
    match exp with
    | Const_bool_exp v -> if v then print_string "true" else print_string "false"
    | Lt_exp (exp1, exp2) -> (print_exp exp1); (print_string " < "); print_exp exp2 
    | And_exp (exp1, exp2) -> (print_exp exp1); (print_string " && "); print_exp exp2
    | Or_exp (exp1, exp2) -> (print_exp exp1); (print_string " || "); print_exp exp2
    | Const_int_exp v -> print_int v
    | Sub_exp (exp1, exp2) -> (print_exp exp1); (print_string " - "); print_exp exp2
    | Add_exp (exp1, exp2) -> (print_exp exp1); (print_string " + "); print_exp exp2
    | Mul_exp (exp1, exp2) -> (print_exp exp1); (print_string " * "); print_exp exp2
    | Div_exp (exp1, exp2) -> (print_exp exp1); (print_string " / "); print_exp exp2
    | Var_exp str -> print_string str
    | Call_exp (str, expli) -> (print_string (str^"( "));
                              (print_expli expli);
                              (print_string ")")
and print_expli = 
    fun expli ->
    match expli with
    | [] -> ()
    | e::el -> (print_exp e);
              (print_string " ");
              (print_expli el)




let rec print_statlist =
    fun statli ->
    match statli with
    | []       -> ()
    | (Var_def_stat (ty, str, exp))::sl -> (print_type ty); 
                                          (print_string (" "^str^" = ")); 
                                          (print_exp exp); 
                                          (print_newline ());
                                          print_statlist sl
    | (Var_set_stat (str, exp))::sl -> (print_string (str^" = "));
                                      (print_exp exp);
                                      (print_newline ());
                                      print_statlist sl
    | (If_stat (exp,stat1,stat2))::sl -> (print_string "if( ");
                                        (print_exp exp);
                                        (print_string " ) {");
                                        (print_newline ());
                                        (print_statlist stat1);
                                        (print_string "} else {");
                                        (print_newline ());
                                        (print_statlist stat2);
                                        (print_newline ());
                                        (print_string "}");
                                        (print_newline ());
                                        print_statlist sl 
    | (While_stat (exp, statli))::sl ->  (print_string "while( ");
                                        (print_exp exp);
                                        (print_string " ) { ");
                                        (print_newline ());
                                        (print_statlist statli);
                                        (print_string "}");
                                        (print_newline ());
                                        print_statlist sl
    | (Print_stat exp)::sl -> (print_string "print( ");
                             (print_exp exp);
                             (print_string " )");
                             (print_newline ());
                             print_statlist sl
    | (Return_stat exp)::sl -> (print_string "return ");
                              (print_exp exp);
                              (print_newline ());
                              print_statlist sl

let rec print_paramlist =
    fun paramli ->
    match paramli with
    | []    -> ()
    | (A_param (str,basety))::pl -> (print_string (str ^ ":")); (print_type basety); (print_string " ");print_paramlist pl 


let print_func = 
    fun func ->
    match func with
    | A_func (ty, name, paramli, statli) ->  (print_type ty); 
                                            (print_string name); 
                                            print_string "(" ; 
                                            (print_paramlist paramli); 
                                            (print_string ")");
                                            (print_char '{');
                                            (print_newline ());
                                            (print_statlist statli);
                                            (print_string "}");
                                            (print_newline ())


let rec print_func_list =
    fun funcList ->
    match funcList with
    | [] -> () 
    | f::fl ->  (print_func f); (print_func_list fl)


let print_pgm = 
    fun pgm -> 
    match pgm with
    | A_program [] -> print_string "no input!"
    | A_program func_list ->  (print_func_list func_list)



(* --------------------------------------test irgen *)

let rec print_irexp =
    fun irexp ->
    match irexp with
    | Ir_constant num       -> print_int num 
    | Ir_var str            -> print_string str
    | Ir_biop (e1, op, e2)  -> print_irexp e1; print_string " op "; print_irexp e2
    | Ir_call name          -> print_string ("call " ^ name)


let print_ircomm =
    fun ircomm ->
    match ircomm with
    | Ir_label num           -> print_string ("_B" ^ (string_of_int num) ^ ":"); print_newline ()
    | Ir_assign (str, irexp) -> print_string (str ^ " = "); print_irexp irexp; print_newline ()
    | Ir_goto num            -> print_string ("goto _B" ^ (string_of_int num)); print_newline ()
    | Ir_ifz (irexp, num)    -> print_string "ifz "; print_irexp irexp; print_string (" goto _B" ^ (string_of_int num) ^ ":"); print_newline ()  
    | Ir_push irexp          -> print_string "push "; print_irexp irexp; print_newline ()
    | Ir_pop str             -> print_string ("pop " ^ str); print_newline ()
    | Ir_print irexp         -> print_string "print "; print_irexp irexp; print_newline ()
    | Ir_ret irexp           -> print_string "ret "; print_irexp irexp; print_newline () 

let print_commarr = 
    fun commarr ->
    for i = 0 to (Array.length commarr) - 1 do
        print_ircomm (Array.get commarr i)
    done

let rec print_irparamli = 
    fun irparamli ->
    match irparamli with
    | [] -> ()
    | p::pl -> (print_string (p^" ")); print_irparamli pl

let print_irfun =
    fun irfun ->
    match irfun with
    | Ir_a_function (name, irparamli, commarr) -> (print_string name);
                                               (print_string "( ");
                                               (print_irparamli irparamli);
                                               (print_string "){\n");
                                               (print_commarr commarr);
                                               (print_string "}\n")

let rec print_irfunli =
    fun irfunli ->
    match irfunli with
    | [] -> ()
    | f::fl -> (print_irfun f); print_irfunli fl

let print_ir = 
    fun ir_pgm ->
    match ir_pgm with
    | Ir_a_program irfunli -> print_irfunli irfunli










(* ----------------------------------------- *)
let _ = print_pgm ast;
        print_string "--------------IR:\n";
        print_ir (irgen ast)
(* ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core src/testParser.native *)
