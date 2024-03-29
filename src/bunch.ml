
exception BunchPathMatch

(* BUNCHES *)
type 'v p =
  | PVar of 'v
  | PConst of float
  | PInfty
[@@deriving show]

type ('a, 'v) bunch =
  | BEmpty
  | BLeaf of 'a
  | BBranch of ('a, 'v) bunch * ('a, 'v) bunch * 'v p
[@@deriving show]

let rec checkShape b1 b2 = match b1, b2 with
  | BEmpty, BEmpty -> true
  | BLeaf _, BLeaf _ -> true
  | BBranch (l1, r1, p1), BBranch (l2, r2, p2) ->
      checkShape l1 l2 && checkShape r1 r2 && p1 = p2
  | _ -> false

let rec map f b = match b with
  | BEmpty -> BEmpty
  | BLeaf x -> BLeaf (f x)
  | BBranch (l, r, p) ->
    BBranch ((map f l), (map f r), p)

let rec map2 f b1 b2 = match b1, b2 with
  | BEmpty, BEmpty  -> BEmpty
  | BLeaf x, BLeaf y -> BLeaf (f x y)
  | BBranch (l1, r1, p1), BBranch (l2, r2, p2) when p1 = p2->
    BBranch ((map2 f l1 l2), (map2 f r1 r2), p1)
  | _ -> raise BunchPathMatch

let rec length ctx = match ctx with
  | BEmpty -> 0
  | BLeaf _ -> 1
  | BBranch (l, r, _) -> length l + length r

type path =
  | PHere
  | PLeft of path
  | PRight of path
[@@deriving show]

let rec index b p = match b, p with
  | BEmpty, PHere -> (print_string "EMPTY, HERE"); None
  | BLeaf x, PHere -> Some x
  | BBranch (b, _, _), PLeft p -> index b p
  | BBranch (_, b, _), PRight p -> index b p
  | _ -> None

let rec replace b p x = match b, p with
  | _, PHere -> BLeaf x
  | BBranch (l, r, p), PLeft pa -> BBranch (replace l pa x, r, p)
  | BBranch (l, r, p), PRight pa -> BBranch (l, replace r pa x, p)
  | _ -> raise BunchPathMatch

let rec place_bunch b p x = match b, p with
  | _, PHere -> x
  | BBranch (l, r, p), PLeft pa -> BBranch (place_bunch l pa x, r, p)
  | BBranch (l, r, p), PRight pa -> BBranch (l, place_bunch r pa x, p)
  | _ -> raise BunchPathMatch

(* IDEA: can we use path's pointing to a BEmpty to replace bunch_partial? *)

(* PARTIAL BUNCHES *)
type ('a, 'v) bunch_partial =
  | BPHole
  | BPHoleLeft of ('a, 'v) bunch_partial * ('a, 'v) bunch * 'v p
  | BPHoleRight of ('a, 'v) bunch * ('a, 'v) bunch_partial * 'v p
[@@deriving show]

let rec fill_hole b x = match b with
  | BPHole -> BLeaf x
  | BPHoleLeft (l, r, p) -> BBranch (fill_hole l x, r, p)
  | BPHoleRight (l, r, p) -> BBranch (l, fill_hole r x, p)

