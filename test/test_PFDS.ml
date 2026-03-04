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
        ; Alcotest.test_case "root is black" `Quick test_root_is_black ] ) ]
