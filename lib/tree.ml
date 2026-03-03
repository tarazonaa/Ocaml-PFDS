type 'a tree = Empty | Node of 'a tree * 'a * 'a tree
let rec insert node = function
    | Empty -> Node (Empty, node, Empty)
    | Node (left, value, right) -> 
        if node > value then Node (left, value, insert node right)
        else if node < value then Node (insert node left, value, right)
        else Node (left, value, right)

