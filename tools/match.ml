type cls = Kw | Kl | Ks | Kd
type op_base =
  | Oadd
  | Osub
  | Omul
type op = cls * op_base

let commutative = function
  | (_, (Oadd | Omul)) -> true
  | (_, _) -> false

let associative = function
  | (_, (Oadd | Omul)) -> true
  | (_, _) -> false

type atomic_pattern =
  | Tmp
  | AnyCon
  | Con of int64

type pattern =
  | Bnr of op * pattern * pattern
  | Atm of atomic_pattern
  | Var of string * atomic_pattern

let rec pattern_match p w =
  match p with
  | Var _ ->
    failwith "variable not allowed"
  | Atm (Tmp) ->
      begin match w with
      | Atm (Con _ | AnyCon) -> false
      | _ -> true
      end
  | Atm (Con _) -> w = p
  | Atm (AnyCon) ->
      not (pattern_match (Atm Tmp) w)
  | Bnr (o, pl, pr) ->
      begin match w with
      | Bnr (o', wl, wr) ->
          o' = o &&
          pattern_match pl wl &&
          pattern_match pr wr
      | _ -> false
      end

type 'a cursor = (* a position inside a pattern *)
  | Bnrl of op * 'a cursor * pattern
  | Bnrr of op * pattern * 'a cursor
  | Top of 'a

let rec fold_cursor c p =
  match c with
  | Bnrl (o, c', p') -> fold_cursor c' (Bnr (o, p, p'))
  | Bnrr (o, p', c') -> fold_cursor c' (Bnr (o, p', p))
  | Top _ -> p

let peel p x =
  let once out (p, c) =
    match p with
    | Var _ -> failwith "variable not allowed"
    | Atm _ -> (p, c) :: out
    | Bnr (o, pl, pr) ->
        (pl, Bnrl (o, c, pr)) ::
        (pr, Bnrr (o, pl, c)) :: out
  in
  let rec go l =
    let l' = List.fold_left once [] l in
    if List.length l' = List.length l
    then l
    else go l'
  in go [(p, Top x)]

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

type 'a state =
  { id: int
  ; seen: pattern
  ; point: ('a cursor) list }

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

let normalize (point: ('a cursor) list) =
  sort_uniq compare point

let nextbnr tmp s1 s2 =
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
    ; point = normalize (l @ tmp)
    }) (group_by_fst (o1 @ o2))

type p = string

module StateSet : sig
  type set
  val create: unit -> set
  val add: set -> p state ->
           [> `Added | `Found ] * p state
  val iter: set -> (p state -> unit) -> unit
  val elems: set -> (p state) list
end = struct
  include Hashtbl.Make(struct
    type t = p state
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
  | K of op * p state * p state

module StateMap = Map.Make(struct
  type t = table_key
  let compare ka kb =
    match ka, kb with
    | K (o, sl, sr), K (o', sl', sr') ->
      compare (o, sl.id, sr.id)
              (o', sl'.id, sr'.id)
end)

type rule =
  { name: string
  ; pattern: pattern
  (* TODO access pattern *)
  }

let generate_table rl =
  let states = StateSet.create () in
  (* initialize states *)
  let ground =
    List.fold_left
      (fun ini r ->
        peel r.pattern r.name @ ini)
      [] rl |>
    group_by_fst
  in
  let find x d l =
    try List.assoc x l with Not_found -> d in
  let tmp = find (Atm Tmp) [] ground in
  let con = find (Atm AnyCon) [] ground in
  let () =
    List.iter (fun (seen, l) ->
      let point =
        if pattern_match (Atm Tmp) seen
        then normalize (tmp @ l)
        else normalize (con @ l)
      in
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
      nextbnr tmp sl sr |>
      List.iter (fun (o, s') ->
        let flag', s' =
          StateSet.add states s' in
        flagmerge flag';
        map_add (K (o, sl, sr)) s';
    ));
  done;
  (StateSet.elems states, !map)

let intersperse x l =
  let rec go left right out =
    let out =
      (List.rev left @ [x] @ right) ::
      out in
    match right with
    | x :: right' ->
      go (x :: left) right' out
    | [] -> out
  in go [] l []

let rec permute = function
  | [] -> [[]]
  | x :: l ->
    List.concat (List.map
      (intersperse x) (permute l))

(* build all binary trees with ordered
 * leaves l *)
let rec bins build l =
  let rec go l r out =
    match r with
    | [] -> out
    | x :: r' ->
      go (l @ [x]) r'
        (fold_pairs
          (bins build l)
          (bins build r)
          out (fun (l, r) out ->
                 build l r :: out))
  in
  match l with
  | [] -> []
  | [x] -> [x]
  | x :: l -> go [x] l []

let products l ini f =
  let rec go acc la = function
    | [] -> f (List.rev la) acc
    | xs :: l ->
      List.fold_left (fun acc x ->
          go acc (x :: la) l)
        acc xs
  in go ini [] l

(* combinatorial nuke... *)
let rec ac_equiv =
  let rec alevel o = function
    | Bnr (o', l, r) when o' = o ->
      alevel o l @ alevel o r
    | x -> [x]
  in function
  | Bnr (o, _, _) as p
  when associative o ->
    products
      (List.map ac_equiv (alevel o p)) []
      (fun choice out ->
        List.map
          (bins (fun l r -> Bnr (o, l, r)))
          (if commutative o
            then permute choice
            else [choice]) |>
        List.concat |>
        (fun l -> List.rev_append l out))
  | Bnr (o, l, r)
  when commutative o ->
    fold_pairs
      (ac_equiv l) (ac_equiv r) []
      (fun (l, r) out ->
        Bnr (o, l, r) ::
        Bnr (o, r, l) :: out)
  | Bnr (o, l, r) ->
    fold_pairs
      (ac_equiv l) (ac_equiv r) []
      (fun (l, r) out ->
        Bnr (o, l, r) :: out)
  | x -> [x]
