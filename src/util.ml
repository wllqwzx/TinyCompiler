
open Core.Std
(*------------------- array operation --------------------- *)

(*let insertAfter = 
    fun arr pos v ->
    let pre = Array.sub arr 0 (pos + 1) in
    let post = Array.sub arr (pos + 1) ((Array.length arr) - (pos + 1)) in
    let t = Array.append pre [|v|] in
    Array.append t post*)

let insertFront =
    fun refarr v ->
    let tt = Array.append [|v|] !refarr in
    refarr := tt

let insertBack =
    fun refarr v ->
    let tt = Array.append !refarr [|v|] in
    refarr := tt


let dropAt =
    fun refarr pos ->
    let pre = Array.sub !refarr 0 pos in
    let post = Array.sub !refarr (pos + 1) ((Array.length !refarr) - (pos + 1)) in
    refarr := Array.append pre post



(*---------------------------------------- *)
