
(*------------------- array operation --------------------- *)

let insertAfter = 
    fun arr pos v ->
    let pre = Array.sub arr 0 (pos + 1) in
    let post = Array.sub arr (pos + 1) ((Array.length arr) - (pos + 1)) in
    let t = Array.append pre [|v|] in
    Array.append t post

let insertFront =
    fun arr v ->
    Array.append [|v|] arr

let insertBack =
    fun arr v ->
    Array.append arr [|v|]

let dropAt =
    fun arr pos ->
    let pre = Array.sub arr 0 pos in
    let post = Array.sub arr (pos + 1) ((Array.length arr) - (pos + 1)) in
    Array.append pre post



(*------------------- ??? --------------------- *)

