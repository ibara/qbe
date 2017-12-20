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
  let once out (p, c) =
    match p with
    | Atm _ -> (p, c) :: out
    | Unr (o, pa) ->
        (pa, Unra (o, c)) :: out
    | Bnr (o, pl, pr) ->
        (pl, Bnrl (o, c, pr)) ::
        (pr, Bnrr (o, pl, c)) :: out
  in
  let rec go l =
    let l' = List.fold_left once [] l in
    if List.length l' = List.length l
    then l
    else go l'
  in go [(p, Top)]

let fold_pairs l1 l2 ini f =
  let rec go acc = function
    | [] -> acc
    | a :: l1' -> 
        go (List.fold_left
          (fun acc b -> f (a, b) acc) 
          acc l2) l1'
  in go ini l1

let iter_pairs l f =
  fold_pairs l l () (fun x () -> f x)

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

let group_by_fst l =
  List.fast_sort (fun (a, _) (b, _) ->
    compare a b) l |>
  List.fold_left (fun (oo, l, res) (o', c) ->
      match oo with
      | None -> (Some o', [c], [])
      | Some o when o = o' -> (oo, c :: l, res)
      | Some o -> (Some o', [c], (o, l) :: res))
    (None, [], []) |>
  (function
    | (None, _, _) -> []
    | (Some o, l, res) -> (o, l) :: res)

let sort_uniq cmp l =
  List.fast_sort cmp l |>
  List.fold_left (fun (eo, l) e' ->
      match eo with
      | None -> (Some e', l)
      | Some e ->
        if cmp e e' = 0
        then (eo, l)
        else (Some e', e :: l)
    ) (None, []) |>
  (function
    | (None, _) -> []
    | (Some e, l) -> List.rev (e :: l))

let normalize (point: cursor list) =
  sort_uniq compare point

let nextbnr any s1 s2 =
  let pm w (_, p) = pattern_match p w in
  let o1 = binops `L s1 |>
           List.filter (pm s2.seen) |>
           List.map fst
  and o2 = binops `R s2 |>
           List.filter (pm s1.seen) |>
           List.map fst
  in
  List.map (fun (o, l) ->
    o,
    { id = 0
    ; seen = Bnr (o, s1.seen, s2.seen)
    ; point = normalize (l @ any)
    }) (group_by_fst (o1 @ o2))

let nextunr any s =
  List.fold_left (fun res -> function
      | Unra (o, c) -> (o, c) :: res
      | _ -> res)
    [] s.point |>
  group_by_fst |>
  List.map (fun (o, l) ->
    o,
    { id = 0
    ; seen = Unr (o, s.seen)
    ; point = normalize (l @ any)
    })

module StateSet : sig
  type set
  val create: unit -> set
  val add: set -> state ->
           [> `Added | `Found ] * state
  val iter: set -> (state -> unit) -> unit
  val elems: set -> state list
end = struct
  include Hashtbl.Make(struct
    type t = state
    let equal s1 s2 = s1.point = s2.point
    let hash s = Hashtbl.hash s.point
  end)
  type set =
    { h: int t
    ; mutable next_id: int }
  let create () =
    { h = create 500; next_id = 1 }
  let add set s =
    (* delete the check later *)
    assert (s.point = normalize s.point);
    try
      let id = find set.h s in
      `Found, {s with id}
    with Not_found -> begin
      let id = set.next_id in
      set.next_id <- id + 1;
      add set.h s id;
      `Added, {s with id}
    end
  let iter set f =
    let f s id = f {s with id} in
    iter f set.h
  let elems set =
    let res = ref [] in
    iter set (fun s -> res := s :: !res);
    !res
end

type table_key =
  | KU of op * state
  | KB of op * state * state

module StateMap = Map.Make(struct
  type t = table_key
  let compare ka kb =
    match ka, kb with
    | KU (_), KB (_) -> -1
    | KB (_), KU (_) -> +1
    | KU (o, s), KU (o', s') ->
      compare (o, s.id) (o', s'.id)
    | KB (o, sl, sr), KB (o', sl', sr') ->
      compare (o, sl.id, sr.id)
              (o', sl'.id, sr'.id)
end)

let generate_table pl =
  let states = StateSet.create () in
  (* initialize states *)
  let ground =
    List.fold_left
      (fun ini p -> peel p @ ini)
      [] pl |>
    group_by_fst
  in
  let any =
    try List.assoc (Atm Any) ground
    with Not_found -> []
  in
  let () =
    List.iter (fun (seen, l) ->
      let point = normalize (any @ l) in
      let s = {id = 0; seen; point} in
      let flag, _ = StateSet.add states s in
      assert (flag = `Added)
    ) ground
  in
  (* setup loop state *)
  let map = ref StateMap.empty in
  let map_add k s' =
    map := StateMap.add k s' !map
  in
  let flag = ref `Added in
  let flagmerge = function
    | `Added -> flag := `Added
    | _ -> ()
  in
  (* iterate until fixpoint *)
  while !flag = `Added do
    flag := `Stop;
    let statel = StateSet.elems states in
    iter_pairs statel (fun (sl, sr) ->
      nextbnr any sl sr |>
      List.iter (fun (o, s') ->
        let flag', s' =
          StateSet.add states s' in
        flagmerge flag';
        map_add (KB (o, sl, sr)) s';
    ));
    statel |>
    List.iter (fun s ->
      nextunr any s |>
      List.iter (fun (o, s') ->
        let flag', s' =
          StateSet.add states s' in
        flagmerge flag';
        map_add (KU (o, s)) s';
    ));
  done;
  (StateSet.elems states, !map)
