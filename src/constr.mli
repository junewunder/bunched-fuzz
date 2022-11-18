open Syntax
open Ctx

open Support.FileInfo

(* Type of constraints generated by the type-checker when verifying
   subtyping. A constraint has the form

     φ ⇒ R ≤ R'

   where φ is a list of equality constraints (S = S') between size
   expressions, and R and R' are sensitivity expressions. *)

type constr = {
  c_info     : info;

  (* Kinding context for the expressions *)
  c_kind_ctx : kind list_ctx;

  (* List of equality constraints between size expressions. Intended
     invariant: all variables have kind Size under the kinding
     context. *)
  c_cs : si_cs list;

  (* The smaller side in the inequality *)
  c_lower : si;

  (* The bigger side in the inequality *)
  c_upper : si;
}
[@@deriving show]

(* Convenience wrapper around constr *)
val mk_constr_leq : info -> context -> si -> si -> constr

val post_si_leq: info -> context -> si -> si -> bool
val post_si_eq : info -> context -> si -> si -> bool

val get_cs : unit -> constr list
