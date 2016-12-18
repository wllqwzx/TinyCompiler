open Core.Std
open IrAst
open Cfg


let hasOptimized = ref true

let performConsProp =
    fun replace funNodes ->
    let consPropForVar =
        fun ~key ~data -> (* key:string, data:int *)
        let var = key in
        let cons = data in
        let consPropForComArr =
            fun ~key ~data ->
            let tempArr = ref [||] in
            for i = 0 to (Array.length !data) - 1 do 
                let comm = !data.(i) in 
                begin
                    match comm with
                    | Ir_assign (str, (Ir_var str1)) ->
                                if (String.compare str1 var) = 0 then
                                        Util.insertBack tempArr (Ir_assign (str, (Ir_constant cons)))
                                else    Util.insertBack tempArr comm    
                    | Ir_assign (str, (Ir_Phi ((Ir_var str1), (Ir_var str2)))) -> 
                                if (String.compare str1 var) = 0 && (String.compare str2 var) = 0 then
                                    Util.insertBack tempArr (Ir_assign (str, (Ir_Phi ((Ir_constant cons), (Ir_constant cons)))))
                                else if (String.equal str1 var) = true && (String.equal str2 var) = false then
                                    Util.insertBack tempArr (Ir_assign (str, (Ir_Phi ((Ir_constant cons), (Ir_var str2)))))
                                else if (String.equal str1 var) = false && (String.equal str2 var) = true then
                                    Util.insertBack tempArr (Ir_assign (str, (Ir_Phi ((Ir_var str1), (Ir_constant cons)))))
                                else    
                                    Util.insertBack tempArr comm
                    | Ir_assign (str, (Ir_Phi ((Ir_var str1), (Ir_constant num)))) -> 
                                if (String.compare str1 var) = 0 then
                                        Util.insertBack tempArr (Ir_assign (str, (Ir_Phi ((Ir_constant cons), (Ir_constant num)))))
                                else    Util.insertBack tempArr comm
                    | Ir_assign (str, (Ir_Phi ((Ir_constant num), (Ir_var str1)))) -> 
                                if (String.compare str1 var) = 0 then
                                        Util.insertBack tempArr (Ir_assign (str, (Ir_Phi ((Ir_constant num), (Ir_constant cons)))))
                                else    Util.insertBack tempArr comm
                    | Ir_assign (str, (Ir_biop ((Ir_var str1), op, (Ir_var str2)))) -> 
                                if (String.compare str1 var) = 0 && (String.compare str2 var) = 0 then
                                    Util.insertBack tempArr (Ir_assign (str, (Ir_biop ((Ir_constant cons), op, (Ir_constant cons)))))
                                else if (String.equal str1 var) = true && (String.equal str2 var) = false then
                                    Util.insertBack tempArr (Ir_assign (str, (Ir_biop ((Ir_constant cons), op, (Ir_var str2)))))
                                else if (String.equal str1 var) = false && (String.equal str2 var) = true then
                                    Util.insertBack tempArr (Ir_assign (str, (Ir_biop ((Ir_var str1), op, (Ir_constant cons)))))
                                else    
                                    Util.insertBack tempArr comm
                    | Ir_assign (str, (Ir_biop ((Ir_var str1), op, (Ir_constant num)))) -> 
                                if (String.compare str1 var) = 0 then
                                        Util.insertBack tempArr (Ir_assign (str, (Ir_biop ((Ir_constant cons), op, (Ir_constant num)))))
                                else    Util.insertBack tempArr comm
                    | Ir_assign (str, (Ir_biop ((Ir_constant num), op, (Ir_var str1)))) -> 
                                if (String.compare str1 var) = 0 then
                                        Util.insertBack tempArr (Ir_assign (str, (Ir_biop ((Ir_constant num), op, (Ir_constant cons)))))
                                else    Util.insertBack tempArr comm
                    | Ir_ifz    ((Ir_var str), num) -> 
                                if (String.compare str var) = 0 then
                                        Util.insertBack tempArr (Ir_ifz ((Ir_constant cons), num))
                                else    Util.insertBack tempArr comm 
                    | Ir_push   (Ir_var str) -> 
                                if (String.compare str var) = 0 then
                                        Util.insertBack tempArr (Ir_push (Ir_constant cons))
                                else    Util.insertBack tempArr comm
                    | Ir_print  (Ir_var str) -> 
                                if (String.compare str var) = 0 then
                                        Util.insertBack tempArr (Ir_print (Ir_constant cons))
                                else    Util.insertBack tempArr comm
                    | Ir_ret    (Ir_var str) -> 
                                if (String.compare str var) = 0 then
                                        Util.insertBack tempArr (Ir_ret (Ir_constant cons))
                                else    Util.insertBack tempArr comm
                    | _ -> Util.insertBack tempArr comm
                end
            done;
            data := !tempArr
        in
        Hashtbl.iteri funNodes consPropForComArr
    in
    Hashtbl.iteri replace consPropForVar



let performCopyProp = 
    fun replace funNodes ->
    let copyPropForVar = 
        fun ~key ~data -> 
        let var = key in
        let toVar = data in
        let copyPropForComArr = 
            fun ~key ~data ->
            let tempArr = ref [||] in
            for i = 0 to (Array.length !data) - 1 do 
                let comm = !data.(i) in 
                begin
                    match comm with
                    | Ir_assign (str, (Ir_var str1)) ->
                                if (String.compare str1 var) = 0 then
                                        Util.insertBack tempArr (Ir_assign (str, (Ir_var toVar)))
                                else    Util.insertBack tempArr comm    
                    | Ir_assign (str, (Ir_Phi ((Ir_var str1), (Ir_var str2)))) -> 
                                if (String.compare str1 var) = 0 && (String.compare str2 var) = 0 then
                                    Util.insertBack tempArr (Ir_assign (str, (Ir_Phi ((Ir_var toVar), (Ir_var toVar)))))
                                else if (String.equal str1 var) = true && (String.equal str2 var) = false then
                                    Util.insertBack tempArr (Ir_assign (str, (Ir_Phi ((Ir_var toVar), (Ir_var str2)))))
                                else if (String.equal str1 var) = false && (String.equal str2 var) = true then
                                    Util.insertBack tempArr (Ir_assign (str, (Ir_Phi ((Ir_var str1), (Ir_var toVar)))))
                                else    
                                    Util.insertBack tempArr comm
                    | Ir_assign (str, (Ir_Phi ((Ir_var str1), (Ir_constant num)))) -> 
                                if (String.compare str1 var) = 0 then
                                        Util.insertBack tempArr (Ir_assign (str, (Ir_Phi ((Ir_var toVar), (Ir_constant num)))))
                                else    Util.insertBack tempArr comm
                    | Ir_assign (str, (Ir_Phi ((Ir_constant num), (Ir_var str1)))) -> 
                                if (String.compare str1 var) = 0 then
                                        Util.insertBack tempArr (Ir_assign (str, (Ir_Phi ((Ir_constant num), (Ir_var toVar)))))
                                else    Util.insertBack tempArr comm
                    | Ir_assign (str, (Ir_biop ((Ir_var str1), op, (Ir_var str2)))) -> 
                                if (String.compare str1 var) = 0 && (String.compare str2 var) = 0 then
                                    Util.insertBack tempArr (Ir_assign (str, (Ir_biop ((Ir_var toVar), op, (Ir_var toVar)))))
                                else if (String.equal str1 var) = true && (String.equal str2 var) = false then
                                    Util.insertBack tempArr (Ir_assign (str, (Ir_biop ((Ir_var toVar), op, (Ir_var str2)))))
                                else if (String.equal str1 var) = false && (String.equal str2 var) = true then
                                    Util.insertBack tempArr (Ir_assign (str, (Ir_biop ((Ir_var str1), op, (Ir_var toVar)))))
                                else    
                                    Util.insertBack tempArr comm
                    | Ir_assign (str, (Ir_biop ((Ir_var str1), op, (Ir_constant num)))) -> 
                                if (String.compare str1 var) = 0 then
                                        Util.insertBack tempArr (Ir_assign (str, (Ir_biop ((Ir_var toVar), op, (Ir_constant num)))))
                                else    Util.insertBack tempArr comm
                    | Ir_assign (str, (Ir_biop ((Ir_constant num), op, (Ir_var str1)))) -> 
                                if (String.compare str1 var) = 0 then
                                        Util.insertBack tempArr (Ir_assign (str, (Ir_biop ((Ir_constant num), op, (Ir_var toVar)))))
                                else    Util.insertBack tempArr comm
                    | Ir_ifz    ((Ir_var str), num) -> 
                                if (String.compare str var) = 0 then
                                        Util.insertBack tempArr (Ir_ifz ((Ir_var toVar), num))
                                else    Util.insertBack tempArr comm 
                    | Ir_push   (Ir_var str) -> 
                                if (String.compare str var) = 0 then
                                        Util.insertBack tempArr (Ir_push (Ir_var toVar))
                                else    Util.insertBack tempArr comm
                    | Ir_print  (Ir_var str) -> 
                                if (String.compare str var) = 0 then
                                        Util.insertBack tempArr (Ir_print (Ir_var toVar))
                                else    Util.insertBack tempArr comm
                    | Ir_ret    (Ir_var str) -> 
                                if (String.compare str var) = 0 then
                                        Util.insertBack tempArr (Ir_ret (Ir_var toVar))
                                else    Util.insertBack tempArr comm
                    | _ -> Util.insertBack tempArr comm
                end
            done;
            data := !tempArr    
        in
        Hashtbl.iteri funNodes copyPropForComArr
    in
    Hashtbl.iteri replace copyPropForVar


let preformConsFold =
    fun replace funNodes ->
    let consFlodForVar = 
        fun ~key ~data -> 
        let var = key in
        let cons = data in
        let consFoldForComArr = 
            fun ~key ~data ->
            let tempArr = ref [||] in
            for i = 0 to (Array.length !data) - 1 do 
                let comm = !data.(i) in 
                begin
                    match comm with
                    | Ir_assign (str, (Ir_var str1)) ->
                                if (String.compare str1 var) = 0 then
                                        Util.insertBack tempArr (Ir_assign (str, (Ir_constant cons)))
                                else    Util.insertBack tempArr comm    
                    | Ir_assign (str, (Ir_Phi ((Ir_var str1), (Ir_var str2)))) -> 
                                if (String.compare str1 var) = 0 && (String.compare str2 var) = 0 then
                                    Util.insertBack tempArr (Ir_assign (str, (Ir_Phi ((Ir_constant cons), (Ir_constant cons)))))
                                else if (String.equal str1 var) = true && (String.equal str2 var) = false then
                                    Util.insertBack tempArr (Ir_assign (str, (Ir_Phi ((Ir_constant cons), (Ir_var str2)))))
                                else if (String.equal str1 var) = false && (String.equal str2 var) = true then
                                    Util.insertBack tempArr (Ir_assign (str, (Ir_Phi ((Ir_var str1), (Ir_constant cons)))))
                                else    
                                    Util.insertBack tempArr comm
                    | Ir_assign (str, (Ir_Phi ((Ir_var str1), (Ir_constant num)))) -> 
                                if (String.compare str1 var) = 0 then
                                        Util.insertBack tempArr (Ir_assign (str, (Ir_Phi ((Ir_constant cons), (Ir_constant num)))))
                                else    Util.insertBack tempArr comm
                    | Ir_assign (str, (Ir_Phi ((Ir_constant num), (Ir_var str1)))) -> 
                                if (String.compare str1 var) = 0 then
                                        Util.insertBack tempArr (Ir_assign (str, (Ir_Phi ((Ir_constant num), (Ir_constant cons)))))
                                else    Util.insertBack tempArr comm
                    | Ir_assign (str, (Ir_biop ((Ir_var str1), op, (Ir_var str2)))) -> 
                                if (String.compare str1 var) = 0 && (String.compare str2 var) = 0 then
                                    Util.insertBack tempArr (Ir_assign (str, (Ir_biop ((Ir_constant cons), op, (Ir_constant cons)))))
                                else if (String.equal str1 var) = true && (String.equal str2 var) = false then
                                    Util.insertBack tempArr (Ir_assign (str, (Ir_biop ((Ir_constant cons), op, (Ir_var str2)))))
                                else if (String.equal str1 var) = false && (String.equal str2 var) = true then
                                    Util.insertBack tempArr (Ir_assign (str, (Ir_biop ((Ir_var str1), op, (Ir_constant cons)))))
                                else    
                                    Util.insertBack tempArr comm
                    | Ir_assign (str, (Ir_biop ((Ir_var str1), op, (Ir_constant num)))) -> 
                                if (String.compare str1 var) = 0 then
                                        Util.insertBack tempArr (Ir_assign (str, (Ir_biop ((Ir_constant cons), op, (Ir_constant num)))))
                                else    Util.insertBack tempArr comm
                    | Ir_assign (str, (Ir_biop ((Ir_constant num), op, (Ir_var str1)))) -> 
                                if (String.compare str1 var) = 0 then
                                        Util.insertBack tempArr (Ir_assign (str, (Ir_biop ((Ir_constant num), op, (Ir_constant cons)))))
                                else    Util.insertBack tempArr comm
                    | Ir_ifz    ((Ir_var str), num) -> 
                                if (String.compare str var) = 0 then
                                        Util.insertBack tempArr (Ir_ifz ((Ir_constant cons), num))
                                else    Util.insertBack tempArr comm 
                    | Ir_push   (Ir_var str) -> 
                                if (String.compare str var) = 0 then
                                        Util.insertBack tempArr (Ir_push (Ir_constant cons))
                                else    Util.insertBack tempArr comm
                    | Ir_print  (Ir_var str) -> 
                                if (String.compare str var) = 0 then
                                        Util.insertBack tempArr (Ir_print (Ir_constant cons))
                                else    Util.insertBack tempArr comm
                    | Ir_ret    (Ir_var str) -> 
                                if (String.compare str var) = 0 then
                                        Util.insertBack tempArr (Ir_ret (Ir_constant cons))
                                else    Util.insertBack tempArr comm
                    | _ -> Util.insertBack tempArr comm
                end
            done;
            data := !tempArr
        in
        Hashtbl.iteri funNodes consFoldForComArr
    in
    Hashtbl.iteri replace consFlodForVar



let scanCommArr =
    fun funNodes -> 
    fun ~key ~data -> (* key:lid, data:ref arr *)
    let replaceConsProp = Hashtbl.create ~hashable:String.hashable () in (* str ---> int *)
    let replaceCopyProp = Hashtbl.create ~hashable:String.hashable () in (* str ---> str *)
    let replaceConsFlod = Hashtbl.create ~hashable:String.hashable () in (* str ---> int *)
    
    let tempArr = ref [||] in
    for i = 0 to (Array.length !data) - 1 do
        let comm = !data.(i) in
        match comm with 
        | Ir_assign (str, (Ir_Phi ((Ir_constant num1), (Ir_constant num2)))) -> 
                    if num1 = num2 then
                    begin
                        Hashtbl.add replaceConsProp str num1; hasOptimized := true; ()
                    end
                    else Util.insertBack tempArr comm
        | Ir_assign (str, (Ir_constant num)) -> 
                    Hashtbl.add replaceConsProp str num; hasOptimized := true; ()
        | _ -> Util.insertBack tempArr comm
    done;
    data := !tempArr;
    performConsProp replaceConsProp funNodes;


    (*let tempArr = ref [||] in
    for i = 0 to (Array.length !data) - 1 do
        let comm = !data.(i) in
        match comm with 
        | Ir_assign (str, (Ir_Phi ((Ir_var str1), (Ir_var str2)))) -> 
                    if (String.compare str1 str2) = 0 then
                    begin
                        Hashtbl.add replaceCopyProp str str1; ()
                    end
                    else Util.insertBack tempArr comm
        | Ir_assign (str, (Ir_var str1)) -> 
                    Hashtbl.add replaceCopyProp str str1; ()
        | _ -> Util.insertBack tempArr comm
    done;
    data := !tempArr;
    performCopyProp replaceCopyProp funNodes;*)

    let hasFind = ref true in
    while !hasFind = true do
        hasFind := false;
        let tempArr = ref [||] in
        for i = 0 to (Array.length !data) - 1 do
            let comm = !data.(i) in
            if(!hasFind = true) then
                Util.insertBack tempArr comm
            else 
                match comm with
                | Ir_assign (str, (Ir_Phi ((Ir_var str1), (Ir_var str2)))) -> 
                    if (String.compare str1 str2) = 0 then
                    begin
                        Hashtbl.add replaceCopyProp str str1; hasFind := true; hasOptimized := true;()
                    end
                    else Util.insertBack tempArr comm
                | Ir_assign (str, (Ir_var str1)) -> 
                            Hashtbl.add replaceCopyProp str str1; hasFind := true; hasOptimized := true;()
                | _ -> Util.insertBack tempArr comm
        done;
        data := !tempArr;
        if !hasFind = true then
            performCopyProp replaceCopyProp funNodes
        else ()
    done;

    let tempArr = ref [||] in
    for i = 0 to (Array.length !data) - 1 do
        let comm = !data.(i) in
        match comm with 
        | Ir_assign (str, (Ir_biop ((Ir_constant num1), op, (Ir_constant num2)))) -> 
                    let res = ref 0 in
                    begin 
                        match op with
                        | Add -> res := (num1 + num2)
                        | Sub -> res := (num1 - num2)
                        | Mul -> res := (num1 * num2)
                        | Div -> res := (num1 / num2)
                        | And -> res := (num1 * num2)
                        | Or  -> if num1 = 1 then res := 1 else if num2 = 1 then res := 1 else res := 0
                        | Lt  -> if num1 < num2 then res := 1 else res := 0
                    end;
                    Hashtbl.add replaceConsFlod str !res; hasOptimized := true; ()
        | _ -> Util.insertBack tempArr comm
    done;
    data := !tempArr;
    preformConsFold replaceConsFlod funNodes



let optimizeFunc =
    fun ~key ~data -> (* key:funName, data:lid ---> ref arr *)
    Hashtbl.iteri data (scanCommArr data)
 

let performOptimizationOverSSA = 
    fun () ->
    while !hasOptimized = true do
        hasOptimized := false;
        Hashtbl.iteri Cfg.funcHtb optimizeFunc
    done