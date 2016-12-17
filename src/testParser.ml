(* testParser.ml transform ast to easy read code and print out to test if the parser is correct *)

open Core.Std
open Lexing

open Parser
open Lexer
open Ast
open IrAst
open IrGen
open Cfg
open ToSSA
open DeadCodeElim
open Optimize
open RemovePhi
(*
let ast =
    match In_channel.input_line stdin with (* read from terminal, we modify it to read from a file *)
    | None -> print_endline "\nGood bye."; exit 0
    | Some line -> let alexbuf = Lexing.from_string line in
                   try
                   Parser.enterPoint Lexer.read alexbuf
                   with
                   | Lexer.LexerError str -> print_string str; exit 1
                   | Parser.Error -> print_string ("Oops!!! parser error with char: " ^ (Lexing.lexeme alexbuf)
                                                     ^ " at: " ^ (Lexer.error_info alexbuf)); exit 1*)


let getAst = 
    fun str ->
    let alexbuf = Lexing.from_string str in
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
    | Ir_Phi ((Ir_var str1), (Ir_var str2))   -> fprintf file "%s" ("Phi(" ^ str1 ^ ", " ^ str2 ^ ")")
    | _ -> print_string "error in testParser:print_irexp!\n"

let print_ircomm =
    fun ircomm file ->
    match ircomm with
    | Ir_label num           -> fprintf file "%s" ("_B" ^ (string_of_int num) ^ ":" ^ "\n");
    | Ir_assign (str, irexp) -> fprintf file "%s" (str ^ " = "); print_irexp irexp file; fprintf file "\n"
    | Ir_goto num            -> fprintf file "%s" ("goto _B" ^ (string_of_int num)); fprintf file "\n"
    | Ir_ifz (irexp, num)    -> fprintf file "%s" "ifz "; print_irexp irexp file; print_string (" goto _B" ^ (string_of_int num) ^ ":"); fprintf file "\n"
    | Ir_push irexp          -> fprintf file "%s" "push "; print_irexp irexp file; fprintf file "\n"
    | Ir_pop str             -> fprintf file "%s" ("pop " ^ str); fprintf file "\n"
    | Ir_print irexp         -> fprintf file "%s" "print "; print_irexp irexp file; fprintf file "\n"
    | Ir_ret irexp           -> fprintf file "ret "; print_irexp irexp file; fprintf file "\n"

let print_commarr = 
    fun commarr file ->
    for i = 0 to (Array.length commarr) - 1 do
        print_ircomm (Array.get commarr i) file
    done

let rec print_irparamli = 
    fun irparamli file ->
    match irparamli with
    | [] -> ()
    | p::pl -> (fprintf file "%s" (p^" ")); print_irparamli pl file

let print_irfun =
    fun irfun file ->
    match irfun with
    | Ir_a_function (name, irparamli, commarr) -> (fprintf file "%s" name);
                                               (fprintf file "( ");
                                               (print_irparamli irparamli file);
                                               (fprintf file "){\n");
                                               (print_commarr commarr file);
                                               (fprintf file "}\n")

let rec print_irfunli =
    fun irfunli file ->
    match irfunli with
    | [] -> ()
    | f::fl -> (print_irfun f file); print_irfunli fl file

let print_ir = 
    fun ir_pgm irFileName ->
    match ir_pgm with
    | Ir_a_program irfunli -> 
        let file = Out_channel.create irFileName in
        print_irfunli irfunli file;
        Out_channel.close file





(* ----------------------------------------- driver *)

let outPutAll = ref false
let fileName = ref ""

let () =
    let len = Array.length Sys.argv in
    if len = 3 then
        outPutAll := true
    else ();
    fileName := Sys.argv.(1);
    let file = In_channel.create !fileName in
    let str = In_channel.input_all file in
    In_channel.close file;
    let ast = getAst str in
    (*print_pgm ast;*)
    (*print_string "--------------IR:\n";*)
    let ir_ast = irgen ast in
    if !outPutAll = true then
        print_ir ir_ast (!fileName ^ ".IR")
    else ();
    (*print_string "--------------SSA:\n";*)
    makeCFG ir_ast;
    makeFatherArray ();
    transToSSA ();
    if !outPutAll = true then
        Cfg.print_CFG (!fileName ^ ".SSA")
    else ();
    
    (*print_string "--------------codeElim\n";*)
    performDeadCodeElim ();
    if !outPutAll = true then
        Cfg.print_CFG (!fileName ^ ".ELIM")
    else ();
    (*print_string "--------------optimize\n";*)
    performOptimizationOverSSA ();
    (*Cfg.print_CFG (!fileName ^ ".SSA");*)
    (*print_string "--------------codeElim\n";*)
    performDeadCodeElim ();
    if !outPutAll = true then
        Cfg.print_CFG (!fileName ^ ".OPT")
    else ();
    (*print_string "--------------removePhi\n";*)
    removePhi ();
    Cfg.print_CFG (!fileName ^ ".FINAL")
(* ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core src/testParser.native *)
