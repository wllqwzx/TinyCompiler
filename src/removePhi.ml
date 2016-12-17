open Core.Std
open IrAst
open Cfg

let removePhiInBlock =
    fun fathersOfFunc funNodes -> 
    fun ~key ~data -> (* key:bid, data: ref arr *)
    let insertAssign = 
        fun bid str exp ->
        let nodeOpt = Hashtbl.find funNodes bid in
        match nodeOpt with
        | None -> print_string "error 1 in insertAssign!\n"
        | Some refarr -> 
            let hasInserted = ref false in
            let tempArr = ref [||] in
            let len = (Array.length !refarr) - 1 in
            for i = 0 to (Array.length !refarr) - 1 do
                let comm = !refarr.(len - i) in
                match comm with
                | Ir_goto id -> Util.insertFront tempArr comm
                | Ir_ifz (exp, id) -> Util.insertFront tempArr comm
                | _ ->  if !hasInserted = false then
                        begin
                            hasInserted := true;
                            Util.insertFront tempArr (Ir_assign (str, exp))
                        end
                        else ();
                        Util.insertFront tempArr comm
            done;
            if !hasInserted = false then
                Util.insertFront tempArr (Ir_assign (str, exp))
            else ();
            refarr := !tempArr
    in    
    let tempArr = ref [||] in
    for i = 0 to (Array.length !data) - 1 do
        let comm = !data.(i) in
        match comm with
        | Ir_assign (str, (Ir_Phi (exp1,exp2))) -> 
                    let fathersOpt = Hashtbl.find fathersOfFunc key in
                    begin
                        match fathersOpt with
                        | None -> print_string "error  1 in removePhiInBlock!\n"
                        | Some refarr -> let len = Array.length !refarr in 
                                            if len = 2 then
                                            begin
                                                insertAssign !refarr.(0) str exp1;
                                                insertAssign !refarr.(1) str exp2
                                            end
                                            else print_string "error 2 in removePhiInBlock\n"
                    end
        | _ -> Util.insertBack tempArr comm
    done;
    data := !tempArr


let removePhiInFunc =
    fun ~key ~data -> (* key:funName, data: lid ---> ref arr *)
    let fathersOpt = Hashtbl.find Cfg.fatherArray key in
    begin
        match fathersOpt with
        | None -> print_string "error  1 in removePhiInBlock!\n"
        | Some fathersOfFunc -> Hashtbl.iteri data (removePhiInBlock fathersOfFunc data)
    end


let removePhi = 
    fun () -> 
    Hashtbl.iteri funcHtb removePhiInFunc