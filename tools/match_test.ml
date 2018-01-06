#use "match.ml"

let test_pattern_match =
  let pm = pattern_match
  and nm = fun x y -> not (pattern_match x y) in
  begin
    assert (nm (Atm Tmp) (Atm (Con 42L)));
    assert (pm (Atm AnyCon) (Atm (Con 42L)));
    assert (nm (Atm (Con 42L)) (Atm AnyCon));
    assert (nm (Atm (Con 42L)) (Atm Tmp));
  end

let test_peel =
  let o = Kw, Oadd in
  let p = Bnr (o, Bnr (o, Atm Tmp, Atm Tmp),
                  Atm (Con 42L)) in
  let l = peel p () in
  let () = assert (List.length l = 3) in
  let atomic_p (p, _) =
    match p with Atm _ -> true | _ -> false in
  let () = assert (List.for_all atomic_p l) in
  let l = List.map (fun (p, c) -> fold_cursor c p) l in
  let () = assert (List.for_all ((=) p) l) in
  ()

let test_fold_pairs =
  let l = [1; 2; 3; 4; 5] in
  let p = fold_pairs l l [] (fun a b -> a :: b) in
  let () = assert (List.length p = 25) in
  let p = sort_uniq compare p in
  let () = assert (List.length p = 25) in
  ()

(* test pattern & state *)
let tp =
  let o = Kw, Oadd in
  Bnr (o, Bnr (o, Atm Tmp, Atm Tmp),
                  Atm (Con 0L))
let ts =
  { id = 0
  ; seen = Atm Tmp
  ; point =
    List.map snd
      (List.filter (fun (p, _) -> p = Atm Tmp)
        (peel tp ()))
  }

let print_sm =
  let op_str (k, o) =
    Printf.sprintf "%s%s"
      (match o with
       | Oadd -> "add"
       | Osub -> "sub"
       | Omul -> "mul")
      (match k with
       | Kw -> "w"
       | Kl -> "l"
       | Ks -> "s"
       | Kd -> "d")
  in
  StateMap.iter (fun k s' ->
    match k with
    | K (o, sl, sr) ->
        Printf.printf
          "(%s %d %d) -> %d\n"
          (op_str o)
          sl.id sr.id s'.id
  )

let address_rules =
  let oa = Kl, Oadd in
  let om = Kl, Omul in
  let rule name pattern =
    List.mapi (fun i pattern ->
        { name = Printf.sprintf "%s%d" name (i+1)
        ; pattern; })
      (ac_equiv pattern) in

    (* o + b *)
  rule "ob" (Bnr (oa, Atm Tmp, Atm AnyCon))
  @ (* b + s * i *)
  rule "bs" (Bnr (oa, Atm Tmp, Bnr (om, Atm AnyCon, Atm Tmp)))
  @ (* o + s * i *)
  rule "os" (Bnr (oa, Atm AnyCon, Bnr (om, Atm AnyCon, Atm Tmp)))
  @ (* b + o + s * i *)
  rule "bos" (Bnr (oa, Bnr (oa, Atm AnyCon, Atm Tmp), Bnr (om, Atm AnyCon, Atm Tmp)))

let sl, sm = generate_table address_rules
let s n = List.find (fun {id; _} -> id = n) sl
let () = print_sm sm

(*
let tp0 =
  let o = Kw, Oadd in
  Bnr (o, Atm Tmp, Atm (Con 0L))
let tp1 =
  let o = Kw, Oadd in
  Bnr (o, tp0, Atm (Con 1L))
*)
