(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)
open Syntax

(* Contexts of type 'a *)
type context =
  {
      var_ctx   : ty bunch_ctx;
      tyvar_ctx : kind list_ctx;
      cs_ctx    : si_cs list;
    } [@@deriving show]

val empty_context : context

val extend_var   : string -> ty -> ?p:p -> context -> context
val extend_var2   : string -> ty -> string -> ty -> ?p:p -> ?q:p -> context -> context
val extend_ty_var : string -> kind -> context -> context
val extend_cs : si_cs -> context -> context

(* val remove_first_var   : context -> context *)
val remove_first_ty_var : context -> context

val access_var    : context -> Bunch.path -> bunch_var * ty
val access_ty_var : context -> int -> list_var * kind

(* Name based functions for the parser *)
val lookup_var   : string -> context -> (bunch_var * ty) option
val lookup_tyvar : string -> context -> (list_var * kind) option
