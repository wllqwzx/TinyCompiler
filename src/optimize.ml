open Core.Std
open IrAst


let performConsProp =
    fun replace ->
    

let performCopyProp = 
    fun replace ->

let preformConsFold =
    fun replace ->




let scanCommArr =
    fun ~key ~data ->
    let tempArr = ref [||] in
    let replaceConsProp = Hashtbl.create ~hashable:String.hashable () in (* str ---> int *)
    let replaceCopyProp = Hashtbl.create ~hashable:String.hashable () in (* str ---> str *)
    let replaceConsFlod = Hashtbl.create ~hashable:String.hashable () in (* str ---> int *)
    for i = 0 to (Array.length !data) - 1 do
        let comm = !data.(i) in
        match comm with 
        | Ir_assign (str, (Ir_Phi ((Ir_constant num1), (Ir_constant num2)))) -> 
                    if num1 = num2 then
                    begin
                        Hashtbl.add replaceConsProp str num1; ()
                    end
                    else Util.insertBack tempArr comm
        | Ir_assign (str, (Ir_Phi ((Ir_var str1), (Ir_var str2)))) -> 
                    if (String.compare str1 str2) = 0 then
                    begin
                        Hashtbl.add replaceCopyProp str str1; ()
                    end
                    else Util.insertBack tempArr comm
        | Ir_assign (str, (Ir_var str1)) -> 
                    Hashtbl.add replaceCopyProp str str1; ()
        | Ir_assign (str, (Ir_constant num)) -> 
                    Hashtbl.add replaceConsProp str num; ()
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
                    Hashtbl.add replaceConsFlod str res; ()
        | _ -> Util.insertBack tempArr comm
    done;
    data := !tempArr;
    performConsProp replaceConsProp;
    performCopyProp replaceCopyProp;
    preformConsFold replaceConsFlod

let optimizeFunc =
    fun ~kay ~data ->
    Hashtbl.iteri data scanCommArr


let performOptimizationOverSSA = 
    fun () ->
    Hashtbl.iteri Cfg.funcHtb optimizeFunc