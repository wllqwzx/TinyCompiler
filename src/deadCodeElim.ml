open Core.Std
open IrAst

let deadCodeElimForFunc = 
    fun ~key ~data -> (* key:funName, data:hashtb for blocks*)
    let varUse = Hashtbl.create ~hashable:String.hashable () in
    
    let makeVarUseInBlock = 
        fun ~key ~data -> (* key:blockId, data:ref arr *)
        for i = 0 to (Array.length !data) - 1 do
            let comm = !data.(i) in
            match comm with
            | Ir_assign (str, (Ir_Phi (str1, str2))) -> 
                        let c0opt = Hashtbl.find varUse str in
                        begin
                        match c0opt with
                        | None -> Hashtbl.add varUse str (ref 0); ()
                        | Some c0 -> ()
                        end;
                        let c1opt = Hashtbl.find varUse str1 in
                        begin
                        match c1opt with
                        | None -> Hashtbl.add varUse str1 (ref 1); ()
                        | Some c1 -> c1 := !c1 + 1
                        end;
                        let c2opt = Hashtbl.find varUse str2 in
                        begin
                        match c2opt with
                        | None -> Hashtbl.add varUse str2 (ref 1); ()
                        | Some c2 -> c2 := !c2 + 1
                        end
                        
            | Ir_assign (str, (Ir_var str1)) -> 
            | Ir_assign (str, (Ir_biop ((Ir_var str1), op, (Ir_var str2)))) -> 
            | Ir_assign (str, (Ir_biop ((Ir_var str1), op, (Ir_constant num)))) -> 
            | Ir_assign (str, (Ir_biop ((Ir_constant num), op, (Ir_var str1)))) -> 
            | Ir_ifz    ((Ir_var str), bid) -> 
            | Ir_push   (Ir_var str) -> 
            | Ir_pop    str -> 
            | Ir_print  (Ir_var str) -> 
            | Ir_ret    (Ir_var str) ->
            | _ -> ()
        done
    in

    let makeVarUse = 
        fun () ->
        Hashtbl.iteri data makeVarUseInBlock 
    in

    let elim = 
        fun () ->
    in

    makeVarUse ();
    elim ()

let performDeadCodeElim = 
    fun () -> 
    Hashtbl.iteri Cfg.funcHtb deadCodeElimForFunc 