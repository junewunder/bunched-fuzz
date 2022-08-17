exception BunchPathMatch

type 'v p =
  | PVar of 'v
  | PConst of float
  | PInfty

type ('a, 'v) bunch =
  | BEmpty
  | BLeaf of 'a
  | BBranch of ('a, 'v) bunch * ('a, 'v) bunch * 'v p

val map : ('a -> 'b) -> ('a, 'v) bunch -> ('b, 'v) bunch
val map2 : ('a -> 'b -> 'c) -> ('a, 'v) bunch -> ('b, 'v) bunch -> ('c, 'v) bunch
val length : ('a, 'v) bunch -> int

type path =
  | PHere
  | PLeft of path
  | PRight of path

val index : ('a, 'v) bunch -> path -> 'a option
val replace : ('a, 'v) bunch -> path -> 'a -> ('a, 'v) bunch
val place_bunch : ('a, 'v) bunch -> path -> ('a, 'v) bunch -> ('a, 'v) bunch


type ('a, 'v) bunch_partial =
  | BPHole
  | BPHoleLeft of ('a, 'v) bunch_partial * ('a, 'v) bunch * 'v p
  | BPHoleRight of ('a, 'v) bunch * ('a, 'v) bunch_partial * 'v p

val fill_hole : ('a, 'v) bunch_partial -> 'a -> ('a, 'v) bunch