type cls = Kw | Kl | Ks | Kd
type op_base =
  | Oadd
  | Osub
  | Omul
type op = cls * op_base

type atomic_pattern =
  | Any
  | Con of int64

type pattern =
  | Bnr of op * pattern * pattern
  | Unr of op * pattern
  | Atm of atomic_pattern

let rec pattern_match p w =
  match p with
  | Atm (Any) -> true
  | Atm (Con _) -> w = p
  | Unr (o, pa) ->
      begin match w with
      | Unr (o', wa) ->
          o' = o &&
          pattern_match pa wa
      | _ -> false
      end
  | Bnr (o, pl, pr) ->
      begin match w with
      | Bnr (o', wl, wr) ->
          o' = o &&
          pattern_match pl wl &&
          pattern_match pr wr
      | _ -> false
      end

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

type cursor = (* a position inside a pattern *)
  | Bnrl of op * cursor * pattern
  | Bnrr of op * pattern * cursor
  | Unra of op * cursor
  | Top

let rec fold_cursor c p =
  match c with
  | Bnrl (o, c', p') -> fold_cursor c' (Bnr (o, p, p'))
  | Bnrr (o, p', c') -> fold_cursor c' (Bnr (o, p', p))
  | Unra (o, c') -> fold_cursor c' (Unr (o, p))
  | Top -> p

let peel p =
  let once out (c, p) =
    match p with
    | Atm _ -> (c, p) :: out
    | Unr (o, pa) ->
        (Unra (o, c), pa) :: out
    | Bnr (o, pl, pr) ->
        (Bnrl (o, c, pr), pl) ::
        (Bnrr (o, pl, c), pr) :: out
  in
  let rec go l =
    let l' = List.fold_left once [] l in
    if List.length l' = List.length l
    then l
    else go l'
  in go [(Top, p)]

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

(* we want to compute all the configurations we could
 * possibly be in when processing a block of instructions;
 * to do so, we start with all the possible cursors for
 * the list of patterns we are given, this will be our
 * main "initial state"; each constant (used in the
 * patterns) also generates a state of its own
 *
 * to create new states we can take pairs of states, and
 * combine them with binary operations, we keep the
 * result if it is non-trivial (non-empty) and new (we
 * have not seen this cursor combination yet); we can
 * also do the same with unary operations
 * *)

let fold_pairs l ini f =
  let rec go acc = function
    | [] -> acc
    | a :: l' -> 
        List.fold_left
          (fun acc b -> f (a, b) acc) 
          (go acc l') l
  in go ini l

type state =
  { id: int
  ; seen: pattern
  ; point: (cursor * pattern) list }

let rec binops side {point; _} =
  List.fold_left (fun res (c, _) ->
    match c, side with
    | Bnrl (o, _, r), `L -> (o, r) :: res
    | Bnrr (o, l, _), `R -> (o, l) :: res
    | _ -> res)
    [] point

let nextbnr s1 s2 =
  let ops =
    let pm w (_, p) = pattern_match p w in
    let o1 = binops `L s1 |>
             List.filter (pm s2.seen) |>
             List.map fst
    and o2 = binops `R s2 |>
             List.filter (pm s1.seen) |>
             List.map fst
    in
    (* intersect the two lists *)
    List.filter (fun o ->
      List.exists ((=) o) o1) o2
  in
  ops











let ts =
  let o = Kw, Oadd in
  let p = Bnr (o, Bnr (o, Atm Any, Atm Any),
                  Atm (Con 42L)) in
  { id = 0
  ; seen = Atm Any
  ; point = peel p
  }
