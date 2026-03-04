let merge_empty () =
  Alcotest.check_raises "Cannot merge empty heap"
    (Failure "Cannot merge empty heap") (fun () ->
      ignore (PFDS.Binomial.merge [Node (1, 1, [])] []) )

let insert_into_empty () =
  let h = PFDS.Binomial.insert 1 [] in
  Alcotest.(check bool) "Heap is not empty" true (h <> []) ;
  Alcotest.(check int) "Min is correct" 1 (PFDS.Binomial.findMin h)

let () =
  let open Alcotest in
  run "PFDS"
    [ ( "Binomial Heaps"
      , [ test_case "Merge empty" `Quick merge_empty
        ; test_case "Insert into empty" `Quick insert_into_empty ] ) ]
