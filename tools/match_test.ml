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
  let atomic_p (_, p) =
    match p with Atm _ -> true | _ -> false in
  let () = assert (List.for_all atomic_p l) in
  let l = List.map (fun (c, p) -> fold_cursor c p) l in
  let () = assert (List.for_all ((=) p) l) in
  ()

let test_fold_pairs =
  let l = [1; 2; 3; 4; 5] in
  let p = fold_pairs l l [] (fun a b -> a :: b) in
  let () = assert (List.length p = 25) in
  let p = List.sort_uniq compare p in
  let () = assert (List.length p = 25) in
  ()

(* test state *)
let ts =
  let o = Kw, Oadd in
  let p = Bnr (o, Bnr (o, Atm Any, Atm Any),
                  Atm (Con 0L)) in
  { id = 0
  ; seen = Atm Any
  ; point =
    List.map fst
      (List.filter (fun (_, p) -> p = Atm Any)
        (peel p))
  }
