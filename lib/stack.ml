type 'a t = 'a list
let empty = []
let isEmpty s = match s with
    [] -> true | s -> false

let cons a b = a :: b
let head s = List.nth s 0
let tail s = List.nth s (List.length s - 1)
let pop s = match s with
    [] -> failwith "Cannot pop from empty stack"
    | h::t -> (h, t)
