module IntHeap = PFDS.Binomial.Make (Int)

let merge_empty () =
  let h = IntHeap.insert 1 [] in
  let merged = IntHeap.merge h [] in
  Alcotest.(check int)
    "Merging empty heap returns the other" 1 (IntHeap.findMin merged)

let insert_into_empty () =
  let h = IntHeap.insert 1 [] in
  Alcotest.(check bool) "Heap is not empty" true (h <> []) ;
  Alcotest.(check int) "Min is correct" 1 (IntHeap.findMin h)

let () =
  let open Alcotest in
  run "PFDS"
    [ ( "Binomial Heaps"
      , [ test_case "Merge empty" `Quick merge_empty
        ; test_case "Insert into empty" `Quick insert_into_empty ] ) ]
