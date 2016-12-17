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

let addLineToDomTree = 
    fun domTree ->
    fun str ->
    let strList = List.filter (String.split str ' ') (fun s -> if phys_equal (String.compare s "")  0 then false else true)in
    let intList = List.map strList (fun s -> Core_kernel.Std_kernel.Int.of_string s) in
    let Some father = List.hd intList in
    let Some tail = List.tl intList in
    let Some child = List.hd tail in 
    let arrOfFather = Hashtbl.find domTree father in
    let arrOfChild = Hashtbl.find domTree child in
    begin
        match arrOfFather with
        | None -> Hashtbl.add domTree father (ref [|child|]); ()
        | Some arrref -> Util.insertBack arrref child
    end;
    begin
        match arrOfChild with
        | None -> Hashtbl.add domTree child (ref [||]); () 
        | Some arrref -> () 
    end

(*----*)

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

let getDomTree = 
    fun () ->
    let domTree = Hashtbl.create ~hashable:Core_kernel.Std_kernel.Int.hashable () in
    let file = In_channel.create ".domTree.out" in
    let Some startStr = In_channel.input_line file in
    let strings = In_channel.input_lines file in
    let start = Core_kernel.Std_kernel.Int.of_string startStr in
    let _ = List.map strings (addLineToDomTree domTree) in 
    In_channel.close file;
    Sys.command "rm .domTree.out";
    (start, domTree)

(* ------------- 2 *)
let addPhiFunc = 
    fun nodeHtb domFrt ->
    let defSite = Hashtbl.create ~hashable:String.hashable () in
    let useSite = Hashtbl.create ~hashable:String.hashable () in
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
    let rec addVarInExpToUseSite =
        fun exp site ->
        match exp with
        | Ir_var    str -> 
               let arrRefOpt = Hashtbl.find useSite str in
               begin
                    match arrRefOpt with
                    | None -> Hashtbl.add useSite str (ref [|site|]); ()
                    | Some arrRef -> Util.insertBack arrRef site 
                end
        | Ir_biop   (exp1, op, exp2) -> addVarInExpToUseSite exp1 site; addVarInExpToUseSite exp2 site
        | Ir_Phi    (exp1, exp2) -> addVarInExpToUseSite exp1 site; addVarInExpToUseSite exp2 site
        | _ -> ()
    in
    let addToUseSite =
        fun ~key ~data ->
        for i = 0 to (Array.length !data) - 1 do
            let comm = !data.(i) in
            match comm with
            | Ir_assign (str, exp) -> addVarInExpToUseSite exp key
            | Ir_ifz    (exp, lid) -> addVarInExpToUseSite exp key
            | Ir_push   exp -> addVarInExpToUseSite exp key
            | Ir_print  exp -> addVarInExpToUseSite exp key
            | Ir_ret    exp -> addVarInExpToUseSite exp key
            | _ -> ()
        done
    in
    let addVarPhiToNode = 
        fun varName node -> 
        let hasAppear = ref false in
        for i = 0 to (Array.length !node) - 1 do
            let comm = !node.(i) in
            match comm with
            | Ir_assign (str, (Ir_Phi ((Ir_var str1), (Ir_var str2)))) -> if (String.compare str varName) = 0 
                                                       then hasAppear := true else ()
            | othercomm -> ();
        done;
        if !hasAppear = true
        then ()
        else Util.insertFront node (Ir_assign (varName, (Ir_Phi ((Ir_var varName), (Ir_var varName)))))
    in
    let addPhiForVar = 
        fun ~key ~data -> (* key:varName, data:def sites *)
        let len = Array.length !data in
        let allsame = ref false in
        if len = 1 then
            let def = !data.(0) in
            let usesOpt = Hashtbl.find useSite key in
            let hasdiff = ref false in
            match usesOpt with
            | None -> allsame := true
            | Some uses -> 
                    begin
                    for p = 0 to (Array.length !uses) - 1 do
                        let ause = !uses.(p) in
                        if ause = def then ()
                        else hasdiff := true
                    done;
                    if !hasdiff = false then
                        allsame := true
                    else ()
                    end
        else ();  
        if !allsame = false then
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
        else ()
    in
    Hashtbl.iteri nodeHtb addToDefSite; (* defSite finished! *)
    Hashtbl.iteri nodeHtb addToUseSite;
    Hashtbl.iteri defSite addPhiForVar





(* --------------- 3*)

let reName =
    fun nodeHtb startNode domTree edgeMat fatherArrayInFunc ->
    let funcVarStack = Hashtbl.create ~hashable:String.hashable () in
    let funcVarCount = Hashtbl.create ~hashable:String.hashable () in
    let initBlock = 
        fun ~key ~data ->
            for i = 0 to (Array.length !data) - 1 do
                let comm = !data.(i) in
                match comm with
                | Ir_assign (str, exp) -> let exist = Hashtbl.find funcVarStack str in
                                         begin
                                         match exist with
                                         | None ->  let stk = Stack.create () in 
                                                   Stack.push stk 0;
                                                   Hashtbl.add funcVarStack str stk;
                                                   Hashtbl.add funcVarCount str (ref 0);()
                                         | Some st -> ()
                                         end 
                | Ir_pop str -> let exist = Hashtbl.find funcVarStack str in
                                begin
                                match exist with
                                | None ->  let stk = Stack.create () in 
                                        Stack.push stk 0;
                                        Hashtbl.add funcVarStack str stk;
                                        Hashtbl.add funcVarCount str (ref 0);()
                                | Some st -> ()
                                end  
                | _ -> ()
            done
    in    
    let rec reNameNode = 
        fun n ->
        let commarrOpt = Hashtbl.find nodeHtb n in
        (* part 1 *)
        begin
        match commarrOpt with
        | None -> print_string "error 1 in reNameNode!"
        | Some commarr -> for i = 0 to (Array.length !commarr) - 1 do
                            let s = !commarr.(i) in
                            begin
                            match s with (* remane vars been used *)
                            | Ir_assign (str, (Ir_Phi ((Ir_var str1), (Ir_var str2)))) -> () 
                            | Ir_assign (str, (Ir_var str1)) -> let Some stk = Hashtbl.find funcVarStack str1 in
                                                               let Some ii = Stack.top stk in
                                                               let n_str1 = str1 ^ "_" ^ (string_of_int ii) in
                                                               Array.set !commarr i (Ir_assign (str, (Ir_var n_str1)))
                            | Ir_assign (str, (Ir_biop ((Ir_var str1), op, (Ir_var str2)))) -> 
                                                               let Some stk1 = Hashtbl.find funcVarStack str1 in
                                                               let Some ii1 = Stack.top stk1 in
                                                               let Some stk2 = Hashtbl.find funcVarStack str2 in
                                                               let Some ii2 = Stack.top stk2 in
                                                               let n_str1 = str1 ^ "_" ^ (string_of_int ii1) in
                                                               let n_str2 = str2 ^ "_" ^ (string_of_int ii2) in
                                                               Array.set !commarr i (Ir_assign (str, (Ir_biop ((Ir_var n_str1), op, (Ir_var n_str2)))))
                            | Ir_assign (str, (Ir_biop ((Ir_var str1), op, (Ir_constant num)))) -> 
                                                               let Some stk = Hashtbl.find funcVarStack str1 in
                                                               let Some ii = Stack.top stk in
                                                               let n_str1 = str1 ^ "_" ^ (string_of_int ii) in
                                                               Array.set !commarr i (Ir_assign (str, (Ir_biop ((Ir_var n_str1), op, (Ir_constant num)))))
                            | Ir_assign (str, (Ir_biop ((Ir_constant num), op, (Ir_var str1)))) -> 
                                                               let Some stk = Hashtbl.find funcVarStack str1 in
                                                               let Some ii = Stack.top stk in
                                                               let n_str1 = str1 ^ "_" ^ (string_of_int ii) in
                                                               Array.set !commarr i (Ir_assign (str, (Ir_biop ((Ir_constant num), op, (Ir_var n_str1)))))
                            | Ir_ifz    ((Ir_var str), id) ->  let Some stk = Hashtbl.find funcVarStack str in
                                                              let Some ii = Stack.top stk in
                                                              let n_str = str ^ "_" ^ (string_of_int ii) in
                                                              Array.set !commarr i (Ir_ifz    ((Ir_var n_str), id))
                            | Ir_push   (Ir_var str) -> let Some stk = Hashtbl.find funcVarStack str in
                                                        let Some ii = Stack.top stk in
                                                        let n_str = str ^ "_" ^ (string_of_int ii) in
                                                        Array.set !commarr i (Ir_push   (Ir_var n_str)) 
                            | Ir_print  (Ir_var str) -> let Some stk = Hashtbl.find funcVarStack str in
                                                        let Some ii = Stack.top stk in
                                                        let n_str = str ^ "_" ^ (string_of_int ii) in
                                                        Array.set !commarr i (Ir_print  (Ir_var n_str))
                            | Ir_ret    (Ir_var str) -> let Some stk = Hashtbl.find funcVarStack str in
                                                        let Some ii = Stack.top stk in
                                                        let n_str = str ^ "_" ^ (string_of_int ii) in
                                                        Array.set !commarr i (Ir_ret    (Ir_var n_str))
                            | _ -> ()
                            end;
                            begin
                            let s = !commarr.(i) in
                            match s with (* rename defined var *)
                            | Ir_assign (str, exp) -> let Some count = Hashtbl.find funcVarCount str in
                                                      let Some stk = Hashtbl.find funcVarStack str in
                                                      count := !count + 1;
                                                      Stack.push stk !count;
                                                      let n_str = str ^ "_" ^ (string_of_int !count) in
                                                      Array.set !commarr i (Ir_assign (n_str, exp))
                            | Ir_pop str ->  let Some count = Hashtbl.find funcVarCount str in
                                            let Some stk = Hashtbl.find funcVarStack str in
                                            count := !count + 1;
                                            Stack.push stk !count;
                                            let n_str = str ^ "_" ^ (string_of_int !count) in
                                            Array.set !commarr i (Ir_pop n_str)
                            | _ -> ()
                            end
                         done
        end;
        (* part 2 *)
        for i = 0 to 99 do
            let y = edgeMat.(n).(i) in
            if y = 1 then
                let Some fatherOfY = Hashtbl.find fatherArrayInFunc i in
                let th = ref (-1) in
                for tt = 0 to (Array.length !fatherOfY) - 1 do
                    if !fatherOfY.(tt) = n then
                        th := tt (* th should be 0 or 1 *)
                    else ()
                done;
                let commarrOptOfI = Hashtbl.find nodeHtb i in
                begin
                match commarrOptOfI with
                | None -> print_string "error 2 in reNameNode!"
                | Some commarrOfI -> for comid = 0 to (Array.length !commarrOfI) - 1 do
                                        let comm = !commarrOfI.(comid) in
                                        begin
                                        match comm with
                                        | Ir_assign (str, (Ir_Phi ((Ir_var str1), (Ir_var str2)))) -> 
                                                if !th = 0 then
                                                    let Some stk = Hashtbl.find funcVarStack str1 in
                                                    let Some ii = Stack.top stk in
                                                    let n_str1 = str1 ^ "_" ^ (string_of_int ii) in
                                                    Array.set !commarrOfI comid (Ir_assign (str, (Ir_Phi ((Ir_var n_str1), (Ir_var str2)))))
                                                else if !th = 1 then
                                                        let Some stk = Hashtbl.find funcVarStack str2 in
                                                        let Some ii = Stack.top stk in
                                                        let n_str2 = str2 ^ "_" ^ (string_of_int ii) in
                                                        Array.set !commarrOfI comid (Ir_assign (str, (Ir_Phi ((Ir_var str1), (Ir_var n_str2)))))
                                                     else print_string "error 3 in reNameNode!" 
                                        | _ -> ()
                                        end
                                    done
                end
            else ()
        done;
        (* part 3 *)
        let Some domedByN = Hashtbl.find domTree n in
        for i = 0 to (Array.length !domedByN) - 1 do
            let x = !domedByN.(i) in
            reNameNode x            
        done;
        (* part 4 *)
        let commarrOpt = Hashtbl.find nodeHtb n in
        begin
        match commarrOpt with
        | None -> print_string "error 1 in reNameNode!"
        | Some commarr -> for i = 0 to (Array.length !commarr) - 1 do
                            let s = !commarr.(i) in
                            begin
                            match s with
                            | Ir_assign (str, exp) -> let posli = String.substr_index_all str false "_" in
                                                     let Some pos = List.last posli in
                                                     let n_str = String.drop_suffix str ((String.length str) -  pos) in
                                                     let Some stk = Hashtbl.find funcVarStack n_str in
                                                     Stack.pop stk; ()
                            | Ir_pop str -> let posli = String.substr_index_all str false "_" in
                                            let Some pos = List.last posli in
                                            let n_str = String.drop_suffix str ((String.length str) -  pos) in
                                            let Some stk = Hashtbl.find funcVarStack n_str in
                                            Stack.pop stk; ()
                            | _ -> ()
                            end
                         done 
        end
    
    in
    Hashtbl.iteri nodeHtb initBlock;
    reNameNode startNode




(* --------------- debug function -------------- *)
let showDomFtr = 
    fun ~(key:Core_kernel.Std_kernel.Int.t Core.Std.Hashtbl.key) ~data ->
    print_int key;
    print_string ": ";
    if phys_equal (Array.length !data) 0 
    then print_string "empty"
    else Array.iter !data (fun num -> print_int num; print_string " ");
    print_newline ()

let showDomTree =
    fun ~key ~data ->
    print_int key;
    print_string ": ";
    if phys_equal (Array.length !data) 0 
    then print_string "empty"
    else Array.iter !data (fun num -> print_int num; print_string " ");
    print_newline ()



(* -------------------------- *)
let transFuncToSSA = 
    fun ~key ~data ->
    let mat = Hashtbl.find Cfg.edgeHtb key in
    match mat with
    | Some edgeMat -> outPutEdge edgeMat;
                     let domFrt = getDomFrontier () in
                     let (startNode, domTree) = getDomTree () in
                     let Some fatherArrayInFunc = Hashtbl.find fatherArray key in
                     (* debug ------ start *)
                     (*print_string "\n ------------------- domFrt \n";
                     Hashtbl.iteri domFrt showDomFtr; 
                     print_string "\n ------------------- domTree \n";
                     print_string "start node is: ";
                     print_int startNode;
                     print_newline ();
                     Hashtbl.iteri domTree showDomTree; *)
                     (* debug ----- end *)
                     addPhiFunc data domFrt;
                     addPhiFunc data domFrt;
                     addPhiFunc data domFrt;
                     addPhiFunc data domFrt;
                     addPhiFunc data domFrt;
                     reName data startNode domTree edgeMat fatherArrayInFunc
    | None -> print_string "impossible!"   


let transToSSA = 
    fun () ->
    Hashtbl.iteri Cfg.funcHtb transFuncToSSA