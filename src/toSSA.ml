open Core.Std
open Lexing
open Array

open Parser
open Lexer
open Ast
open IrAst
open Util
open Cfg

(* ------------ 1 *)
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
    let strList = List.filter (String.split str ' ') (fun s -> if phys_equal (String.compare s "")  0 then false else true)in
    let intList = List.map strList (fun s -> Core_kernel.Std_kernel.Int.of_string s) in
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

(* ------------- 2 *)
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



let addPhiFunc = 
    fun nodeHtb domFrt ->
    let defSite = Hashtbl.create ~hashable:String.hashable () in
    let addVarToDefSite =
        fun varName site ->
        let arrRefOpt = Hashtbl.find defSite varName in
        match arrRefOpt with
        | None -> Hashtbl.add defSite varName (ref [|site|]); ()
        | Some arrRef -> Util.insertBack arrRef site 
    in
    let addToDefSite =
        fun ~key ~data ->
        for i = 0 to (Array.length !data) - 1 do
            let comm = !data.(i) in 
            match comm with
            | Ir_assign (str, exp) -> addVarToDefSite str key
            | Ir_pop str -> addVarToDefSite str key
            | _ -> () 
        done        
    in
    let addVarPhiToNode = 
        fun varName node -> 
        let hasAppear = ref false in
        for i = 0 to (Array.length !node) - 1 do
            let comm = !node.(i) in
            match comm with
            | Ir_assign (str, (Ir_Phi (str1, str2))) -> if (String.compare str varName) = 0 
                                                       then hasAppear := true else ()
            | othercomm -> ();
        done;
        if !hasAppear = true
        then ()
        else Util.insertFront node (Ir_assign (varName, (Ir_Phi (varName, varName))))
    in
    let addPhiForVar = 
        fun ~key ~data ->
        for i = 0 to (Array.length !data) - 1 do
            let site = !data.(i) in
            let siteDomFtr = Hashtbl.find domFrt site in
            match siteDomFtr with
            | None -> print_string "impossible!"
            | Some arrRef -> for j = 0 to (Array.length !arrRef) - 1 do
                                let t = !arrRef.(j) in
                                let curNodeOpt = Hashtbl.find nodeHtb t in
                                match curNodeOpt with
                                | None -> print_string "impossible!"
                                | Some curNode -> addVarPhiToNode key curNode
                            done 
        done
    in
    Hashtbl.iteri nodeHtb addToDefSite; (* defSite finished! *)
    Hashtbl.iteri defSite addPhiForVar





(* --------------- 3*)
let reName =
    fun nodeHtb ->
    


(* --------------- debug function *)
let showDomFtr = 
    fun ~(key:Core_kernel.Std_kernel.Int.t Core.Std.Hashtbl.key) ~data ->
    print_int key;
    print_string ": ";
    if phys_equal (Array.length !data) 0 
    then print_string "empty"
    else Array.iter !data (fun num -> print_int num; print_string " ");
    print_newline ()


let transFuncToSSA = 
    fun ~key ~data ->
    let mat = Hashtbl.find Cfg.edgeHtb key in
    match mat with
    | Some edgeMat -> outPutEdge edgeMat;
                     let domFrt = getDomFrontier () in
                     print_string "\n ------------------- domFrt \n";
                     Hashtbl.iteri domFrt showDomFtr; (* debug *)
                     addPhiFunc data domFrt;
                     reName data
    | None -> print_string "impossible!"   




let transToSSA = 
    fun () ->
    Hashtbl.iteri Cfg.funcHtb transFuncToSSA