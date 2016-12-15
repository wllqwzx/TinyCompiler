open Core.Std
open IrAst

let deadCodeElimForFunc = 
    fun ~key ~data -> (* key:funName, data:hashtb for blocks*)
    let varUse = Hashtbl.create ~hashable:String.hashable () in
    let hasDeleted = ref true in
    let funNodes = data in

    let makeVarUseInBlock = 
        fun ~key ~data -> (* key:blockId, data:ref arr *)
        for i = 0 to (Array.length !data) - 1 do
            let comm = !data.(i) in
            match comm with
            | Ir_assign (str, (Ir_Phi ((Ir_var str1), (Ir_var str2)))) -> 
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
                        end
            | Ir_assign (str, (Ir_biop ((Ir_var str1), op, (Ir_var str2)))) -> 
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
            | Ir_assign (str, (Ir_biop ((Ir_var str1), op, (Ir_constant num)))) -> 
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
                        end
            | Ir_assign (str, (Ir_biop ((Ir_constant num), op, (Ir_var str1)))) ->
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
                        end 
            | Ir_ifz    ((Ir_var str), bid) -> 
                        let copt = Hashtbl.find varUse str in
                        begin
                        match copt with
                        | None -> Hashtbl.add varUse str (ref 1); ()
                        | Some c1 -> c1 := !c1 + 1
                        end 
            | Ir_push   (Ir_var str) -> 
                        let copt = Hashtbl.find varUse str in
                        begin
                        match copt with
                        | None -> Hashtbl.add varUse str (ref 1); ()
                        | Some c1 -> c1 := !c1 + 1
                        end
            | Ir_pop    str -> 
                        let c0opt = Hashtbl.find varUse str in
                        begin
                        match c0opt with
                        | None -> Hashtbl.add varUse str (ref 0); ()
                        | Some c0 -> ()
                        end
            | Ir_print  (Ir_var str) ->
                        let copt = Hashtbl.find varUse str in
                        begin
                        match copt with
                        | None -> Hashtbl.add varUse str (ref 1); ()
                        | Some c1 -> c1 := !c1 + 1
                        end 
            | Ir_ret    (Ir_var str) ->
                        let copt = Hashtbl.find varUse str in
                        begin
                        match copt with
                        | None -> Hashtbl.add varUse str (ref 1); ()
                        | Some c1 -> c1 := !c1 + 1
                        end
            | _ -> ()
        done
    in

    let makeVarUse = 
        fun () ->
        Hashtbl.iteri data makeVarUseInBlock 
    in

    let deleteVarDef = 
        fun var ->
        fun ~key ~data -> (* key:lid, data:ref arr *)
        let newarr = ref [||] in
        for i = 0 to (Array.length !data) - 1 do
            let comm = !data.(i) in
            match comm with
            | Ir_assign (str, (Ir_var str1)) -> 
                        if (String.compare str var) = 0 then
                            begin
                            let c1opt = Hashtbl.find varUse str1 in
                            match c1opt with
                            | None -> print_string "error 1 in deleteVarDef!\n" 
                            | Some c1 -> c1 := !c1 - 1
                            end
                        else Util.insertBack newarr comm
            | Ir_assign (str, (Ir_Phi ((Ir_var str1), (Ir_var str2)))) -> 
                        if (String.compare str var) = 0 then
                        begin
                            let c1opt = Hashtbl.find varUse str1 in
                            begin
                            match c1opt with
                            | None -> print_string "error 2 in deleteVarDef!\n" 
                            | Some c1 -> c1 := !c1 - 1
                            end;
                            let c2opt = Hashtbl.find varUse str2 in
                            begin
                            match c2opt with
                            | None -> print_string "error 3 in deleteVarDef!\n" 
                            | Some c2 -> c2 := !c2 - 1
                            end
                        end
                        else Util.insertBack newarr comm
            | Ir_assign (str, (Ir_biop ((Ir_var str1), op, (Ir_var str2)))) -> 
                        if (String.compare str var) = 0 then
                        begin
                            let c1opt = Hashtbl.find varUse str1 in
                            begin
                            match c1opt with
                            | None -> print_string "error 4 in deleteVarDef!\n" 
                            | Some c1 -> c1 := !c1 - 1
                            end;
                            let c2opt = Hashtbl.find varUse str2 in
                            begin
                            match c2opt with
                            | None -> print_string "error 5 in deleteVarDef!\n" 
                            | Some c2 -> c2 := !c2 - 1
                            end
                        end
                        else Util.insertBack newarr comm
            | Ir_assign (str, (Ir_biop ((Ir_var str1), op, (Ir_constant num)))) ->
                        if (String.compare str var) = 0 then
                        begin
                            let c1opt = Hashtbl.find varUse str1 in
                            match c1opt with
                            | None -> print_string "error 6 in deleteVarDef!\n" 
                            | Some c1 -> c1 := !c1 - 1
                        end
                        else Util.insertBack newarr comm
            | Ir_assign (str, (Ir_biop ((Ir_constant num), op, (Ir_var str1)))) ->
                        if (String.compare str var) = 0 then
                        begin
                            let c1opt = Hashtbl.find varUse str1 in
                            match c1opt with
                            | None -> print_string "error 7 in deleteVarDef!\n" 
                            | Some c1 -> c1 := !c1 - 1
                        end
                        else Util.insertBack newarr comm
            | _ -> Util.insertBack newarr comm
        done; 
        data := !newarr
    in

    let elimVarIfNoUse = 
        fun ~key ~data -> (* key:varName, data: ref int *)
        if !data = 0 then
        begin
            data := -1;
            hasDeleted := true;
            Hashtbl.iteri funNodes (deleteVarDef key)
        end
        else ()
    in

    let elim = 
        fun () ->
        hasDeleted := false;
        Hashtbl.iteri varUse elimVarIfNoUse
    in

    makeVarUse ();
    while !hasDeleted = true do
        elim ()
    done


let performDeadCodeElim = 
    fun () -> 
    Hashtbl.iteri Cfg.funcHtb deadCodeElimForFunc