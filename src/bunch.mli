exception BunchPathMatch

type 'a bunch =
  | BEmpty
  | BLeaf of 'a
  | BBranch of 'a bunch * 'a bunch * float

val map : ('a -> 'b) -> 'a bunch -> 'b bunch
val map2 : ('a -> 'b -> 'c) -> 'a bunch -> 'b bunch -> 'c bunch
val length : 'a bunch -> int

type path =
  | PHere
  | PLeft of path
  | PRight of path

val index : 'a bunch -> path -> 'a option
val replace : 'a bunch -> path -> 'a -> 'a bunch
val place_bunch : 'a bunch -> path -> 'a bunch -> 'a bunch


type 'a bunch_partial =
  | BPHole
  | BPHoleLeft of 'a bunch_partial * 'a bunch * float
  | BPHoleRight of 'a bunch * 'a bunch_partial * float

val fill_hole : 'a bunch_partial -> 'a -> 'a bunch