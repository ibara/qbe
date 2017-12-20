#use "match.ml"

let test_pattern_match =
  let pm = pattern_match
  and nm = fun x y -> not (pattern_match x y)
  and o = (Kw, Oadd) in
  begin
    assert (pm (Atm Any) (Atm (Con 42L)));
    assert (pm (Atm Any) (Unr (o, Atm Any)));
    assert (nm (Atm (Con 42L)) (Atm Any));
    assert (pm (Unr (o, Atm Any))
               (Unr (o, Atm (Con 42L))));
    assert (nm (Unr (o, Atm Any))
               (Unr ((Kl, Oadd), Atm (Con 42L))));
    assert (nm (Unr (o, Atm Any))
               (Bnr (o, Atm (Con 42L), Atm Any)));
  end

let test_peel =
  let o = Kw, Oadd in
  let p = Bnr (o, Bnr (o, Atm Any, Atm Any),
                  Atm (Con 42L)) in
  let l = peel p in
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
  Bnr (o, Bnr (o, Atm Any, Atm Any),
                  Atm (Con 0L))
let ts =
  { id = 0
  ; seen = Atm Any
  ; point =
    List.map snd
      (List.filter (fun (p, _) -> p = Atm Any)
        (peel tp))
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
    | KU (o, s) ->
        Printf.printf
          "(%s %d) -> %d\n"
          (op_str o)
          s.id s'.id
    | KB (o, sl, sr) ->
        Printf.printf
          "(%s %d %d) -> %d\n"
          (op_str o)
          sl.id sr.id s'.id
  )

let address_patterns =
  let oa = Kl, Oadd in
  let om = Kl, Omul in
  (* o + b *)
  [ Bnr (oa, Atm Any, Atm (Con 0L))
  ; Bnr (oa, Atm (Con 0L), Atm Any)

  (* b + s * i *)
  ; Bnr (oa, Atm Any, Bnr (om, Atm (Con 0L), Atm Any))
  ; Bnr (oa, Atm Any, Bnr (om, Atm Any, Atm (Con 0L)))
  ; Bnr (oa, Bnr (om, Atm (Con 0L), Atm Any), Atm Any)
  ; Bnr (oa, Bnr (om, Atm Any, Atm (Con 0L)), Atm Any)

  (* o + s * i *)
  ; Bnr (oa, Atm (Con 0L), Bnr (om, Atm (Con 0L), Atm Any))
  ; Bnr (oa, Atm (Con 0L), Bnr (om, Atm Any, Atm (Con 0L)))
  ; Bnr (oa, Bnr (om, Atm (Con 0L), Atm Any), Atm (Con 0L))
  ; Bnr (oa, Bnr (om, Atm Any, Atm (Con 0L)), Atm (Con 0L))
  ]
