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

let fold_pairs l1 l2 ini f =
  let rec go acc = function
    | [] -> acc
    | a :: l1' -> 
        go (List.fold_left
          (fun acc b -> f (a, b) acc) 
          acc l2) l1'
  in go ini l1

type state =
  { id: int
  ; seen: pattern
  ; point: cursor list }

let rec binops side {point; _} =
  List.fold_left (fun res c ->
    match c, side with
    | Bnrl (o, c, r), `L -> ((o, c), r) :: res
    | Bnrr (o, l, c), `R -> ((o, c), l) :: res
    | _ -> res)
    [] point

let nextbnr s1 s2 =
  let pm w (_, p) = pattern_match p w in
  let o1 = binops `L s1 |>
           List.filter (pm s2.seen) |>
           List.map fst
  and o2 = binops `R s2 |>
           List.filter (pm s1.seen) |>
           List.map fst
  in
  o1 @ o2 |>
  (* group by operation... *)
  List.sort (fun (a, _) (b, _) ->
    compare a b) |>
  List.fold_left (fun (oo, l, res) (o', c) ->
      match oo with
      | None -> (Some o', [c], [])
      | Some o when o = o' -> (oo, c :: l, res)
      | Some o -> (Some o', [c], (o, l) :: res))
    (None, [], []) |>
  (fun (oo, l, res) ->
    match oo with
    | None -> []
    | Some o -> (o, l) :: res) |>
  (* create states *)
  List.map (fun (o, l) ->
    { id = 0
    ; seen = Bnr (o, s1.seen, s2.seen)
    ; point = List.sort_uniq compare l
    })
