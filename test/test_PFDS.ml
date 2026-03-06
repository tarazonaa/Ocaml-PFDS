module IntHeap = PFDS.Binomial.Make (Int)

let merge_empty () =
  let h = IntHeap.insert 1 [] in
  let merged = IntHeap.merge h [] in
  Alcotest.(check (result int string))
    "Merging empty heap returns the other" (Ok 1) (IntHeap.findMin merged)

let insert_into_empty () =
  let h = IntHeap.insert 1 [] in
  Alcotest.(check bool) "Heap is not empty" true (h <> []) ;
  Alcotest.(check (result int string))
    "Min is correct" (Ok 1) (IntHeap.findMin h)

module IntRB = PFDS.Redblack.Make (Int)

let insert_exn x h =
  match IntRB.insert x h with
  | Ok h ->
      h
  | Error e ->
      Alcotest.failf "insert failed: %s" e

let test_empty () =
  let h = IntRB.empty () in
  Alcotest.(check bool)
    "member in empty is error" true
    (Result.is_error (IntRB.member 1 h))

let test_insert_member () =
  let h = IntRB.empty () |> insert_exn 1 in
  Alcotest.(check bool)
    "member finds inserted element" true
    (Result.is_ok (IntRB.member 1 h))

let test_member_not_found () =
  let h = IntRB.empty () |> insert_exn 1 in
  Alcotest.(check bool)
    "member returns error for missing" true
    (Result.is_error (IntRB.member 2 h))

let test_multiple_inserts () =
  let values = [5; 3; 7; 1; 4; 6; 8] in
  let h =
    List.fold_left (fun acc x -> insert_exn x acc) (IntRB.empty ()) values
  in
  List.iter
    (fun x ->
      Alcotest.(check bool)
        (Printf.sprintf "member finds %d" x)
        true
        (Result.is_ok (IntRB.member x h)) )
    values

let test_root_is_black () =
  let h = IntRB.empty () |> insert_exn 1 |> insert_exn 2 |> insert_exn 3 in
  match h with
  | IntRB.Node (IntRB.Black, _, _, _) ->
      ()
  | _ ->
      Alcotest.fail "root is not black"

module IntStream = PFDS.Streams.Make (Int)

let stream_of_list lst =
  List.fold_right
    (fun x acc -> lazy (IntStream.Cons (x, acc)))
    lst
    (lazy IntStream.Nil)

let to_list s =
  let rec aux acc s =
    match Lazy.force s with
    | IntStream.Nil ->
        List.rev acc
    | IntStream.Cons (x, s') ->
        aux (x :: acc) s'
  in
  aux [] s

let test_stream_append () =
  let s1 = stream_of_list [1; 2; 3] in
  let s2 = stream_of_list [4; 5; 6] in
  let s = IntStream.(s1 ++ s2) in
  Alcotest.(check (list int))
    "append two streams" [1; 2; 3; 4; 5; 6] (to_list s)

let test_stream_take () =
  let s = stream_of_list [1; 2; 3; 4; 5] in
  Alcotest.(check (list int)) "take 3" [1; 2; 3] (to_list (IntStream.take 3 s))

let test_stream_drop () =
  let s = stream_of_list [1; 2; 3; 4; 5] in
  Alcotest.(check (list int)) "drop 2" [3; 4; 5] (to_list (IntStream.drop 2 s))

let test_stream_reverse () =
  let s = stream_of_list [1; 2; 3] in
  Alcotest.(check (list int))
    "reverse" [3; 2; 1]
    (to_list (IntStream.reverse s))

module IntQueue = PFDS.Batched.Make (Int)

let test_queue_empty () =
  Alcotest.(check bool)
    "empty queue is empty" true
    (IntQueue.isEmpty IntQueue.empty)

let test_queue_snoc_head () =
  let q = IntQueue.snoc 1 IntQueue.empty in
  Alcotest.(check int) "head after snoc is 1" 1 (IntQueue.head q)

let test_queue_tail () =
  let q = IntQueue.snoc 1 IntQueue.empty in
  let q = IntQueue.snoc 2 q in
  let q = IntQueue.tail q in
  Alcotest.(check int) "head after tail is 2" 2 (IntQueue.head q)

let test_queue_fifo () =
  let q = List.fold_left (fun acc x -> IntQueue.snoc x acc) IntQueue.empty [1; 2; 3; 4; 5] in
  let rec to_list q =
    if IntQueue.isEmpty q then []
    else IntQueue.head q :: to_list (IntQueue.tail q)
  in
  Alcotest.(check (list int)) "fifo order" [1; 2; 3; 4; 5] (to_list q)

let test_queue_head_empty () =
  Alcotest.check_raises "head of empty raises" (Failure "Empty queue.")
    (fun () -> ignore (IntQueue.head IntQueue.empty) )

module IntDeque = PFDS.Deque.Make (Int)

let test_deque_empty () =
  Alcotest.(check bool) "empty deque is empty" true
    (IntDeque.isEmpty IntDeque.empty)

let test_deque_snoc_head () =
  let q = IntDeque.snoc 1 IntDeque.empty in
  Alcotest.(check int) "head after snoc" 1 (IntDeque.head q)

let test_deque_cons_head () =
  let q = IntDeque.snoc 2 IntDeque.empty in
  let q = IntDeque.cons 1 q in
  Alcotest.(check int) "head after cons is 1" 1 (IntDeque.head q)

let test_deque_last () =
  let q = IntDeque.snoc 1 IntDeque.empty in
  let q = IntDeque.snoc 2 q in
  Alcotest.(check int) "last is 2" 2 (IntDeque.last q)

let test_deque_init () =
  let q = IntDeque.snoc 1 IntDeque.empty in
  let q = IntDeque.snoc 2 q in
  let q = IntDeque.init q in
  Alcotest.(check int) "last after init is 1" 1 (IntDeque.last q)

let test_deque_tail () =
  let q = IntDeque.snoc 1 IntDeque.empty in
  let q = IntDeque.snoc 2 q in
  let q = IntDeque.tail q in
  Alcotest.(check int) "head after tail is 2" 2 (IntDeque.head q)

let test_deque_head_empty () =
  Alcotest.check_raises "head of empty raises" (Failure "Deque is empty")
    (fun () -> ignore (IntDeque.head IntDeque.empty))

let test_deque_last_empty () =
  Alcotest.check_raises "last of empty raises" (Failure "Deque is empty")
    (fun () -> ignore (IntDeque.last IntDeque.empty))

module IntSplay = PFDS.Splay.Make (Int)

let splay_of_list lst =
  List.fold_left (fun t x -> IntSplay.insert x t) IntSplay.empty lst

let rec splay_to_list t =
  if t = IntSplay.empty then []
  else
    let x = IntSplay.findMin t in
    x :: splay_to_list (IntSplay.deleteMin t)

let test_splay_insert () =
  let t = splay_of_list [3; 1; 2] in
  Alcotest.(check int) "findMin after inserts" 1 (IntSplay.findMin t)

let test_splay_sorted () =
  let t = splay_of_list [5; 3; 1; 4; 2] in
  Alcotest.(check (list int)) "elements in sorted order" [1; 2; 3; 4; 5]
    (splay_to_list t)

let test_splay_delete_min () =
  let t = splay_of_list [3; 1; 2] in
  let t = IntSplay.deleteMin t in
  Alcotest.(check int) "findMin after deleteMin" 2 (IntSplay.findMin t)

let test_splay_partition () =
  let t = splay_of_list [1; 2; 3; 4; 5] in
  let small, big = IntSplay.partition 3 t in
  Alcotest.(check int) "max of small <= 3" 3 (IntSplay.findMin (IntSplay.deleteMin (IntSplay.deleteMin small)));
  Alcotest.(check int) "min of big > 3" 4 (IntSplay.findMin big)

let () =
  Alcotest.run "PFDS"
    [ ( "Binomial Heaps"
      , [ Alcotest.test_case "Merge empty" `Quick merge_empty
        ; Alcotest.test_case "Insert into empty" `Quick insert_into_empty ] )
    ; ( "Red-Black Tree"
      , [ Alcotest.test_case "empty member" `Quick test_empty
        ; Alcotest.test_case "insert and member" `Quick test_insert_member
        ; Alcotest.test_case "member not found" `Quick test_member_not_found
        ; Alcotest.test_case "multiple inserts" `Quick test_multiple_inserts
        ; Alcotest.test_case "root is black" `Quick test_root_is_black ] )
    ; ( "Batched Queue"
      , [ Alcotest.test_case "empty" `Quick test_queue_empty
        ; Alcotest.test_case "snoc and head" `Quick test_queue_snoc_head
        ; Alcotest.test_case "tail" `Quick test_queue_tail
        ; Alcotest.test_case "fifo order" `Quick test_queue_fifo
        ; Alcotest.test_case "head of empty raises" `Quick test_queue_head_empty
        ] )
    ; ( "Deque"
      , [ Alcotest.test_case "empty" `Quick test_deque_empty
        ; Alcotest.test_case "snoc and head" `Quick test_deque_snoc_head
        ; Alcotest.test_case "cons and head" `Quick test_deque_cons_head
        ; Alcotest.test_case "last" `Quick test_deque_last
        ; Alcotest.test_case "init" `Quick test_deque_init
        ; Alcotest.test_case "tail" `Quick test_deque_tail
        ; Alcotest.test_case "head of empty raises" `Quick test_deque_head_empty
        ; Alcotest.test_case "last of empty raises" `Quick test_deque_last_empty
        ] )
    ; ( "Splay Tree"
      , [ Alcotest.test_case "insert findMin" `Quick test_splay_insert
        ; Alcotest.test_case "sorted order" `Quick test_splay_sorted
        ; Alcotest.test_case "deleteMin" `Quick test_splay_delete_min
        ; Alcotest.test_case "partition" `Quick test_splay_partition
        ] )
    ; ( "Streams"
      , [ Alcotest.test_case "append" `Quick test_stream_append
        ; Alcotest.test_case "take" `Quick test_stream_take
        ; Alcotest.test_case "drop" `Quick test_stream_drop
        ; Alcotest.test_case "reverse" `Quick test_stream_reverse ] ) ]
