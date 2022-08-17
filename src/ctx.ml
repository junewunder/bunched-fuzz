(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)
open Bunch
open Syntax

(* ---------------------------------------------------------------------- *)
(* Context management *)
(* Term and type variables *)
type context =
    {
      var_ctx   : ty bunch_ctx;
      tyvar_ctx : kind list_ctx;
      cs_ctx    : si_cs list;
    }

let length ctx = List.length ctx

let empty_context = { var_ctx = BEmpty; tyvar_ctx = []; cs_ctx = []; }

let rec blookup id ctx = match ctx with
  | BEmpty -> None
  | BLeaf (var, value) ->
    if var.v_name = id then
        Some (var, value)
      else
        None
  | BBranch (l, r, _) -> Option.(
    let l = blookup id l in
    let r = blookup id r in
    (* if is_some l && is_some r then exit 0 *)
    (* else *)
    if is_some l then l
    else if is_some r then r
    else None
  )

(* Return a binding if it exists. Let the caller handle the error *)
let rec slookup id ctx =
  match ctx with
      []                -> None
    | (var, value) :: l ->
      if var.v_name = id then
        Some (var, value)
      else
        slookup id l

let lookup_var id ctx =
  blookup id ctx.var_ctx

let lookup_tyvar id ctx =
  slookup id ctx.tyvar_ctx

(* Context shifting *)

(* Shifting of bindings, this corresponds to a new type being introduced *)
let varctx_ty_shift n d ctx =
  Bunch.map (fun (v, ty) -> (v, ty_shift n d ty)) ctx

(* Shifting of v_names, this is mostly for debug and corresponds to a new abstraction *)
let varctx_var_shift ctx =
  Bunch.map (fun (v, ty) -> (var_shift_left v, ty)) ctx

(* When we introduce a type var, all variables of the type context
   must be shifted *)
let tyvar_ctx_shift n d ctx =
  List.map (fun (v, k) -> (var_shift n d v, k)) ctx

let cs_ctx_shift n d =
  List.map (cs_shift n d)

(* Extend the context with a new variable binding. We just shift term variables *)
let extend_var id bi ?(p=PConst 1.0) ctx =
  let n_var = {
    v_name  = id;
    v_type  = BiVar;
    v_index = PRight PHere;
    v_size  = (Bunch.length ctx.var_ctx) + 1;
  } in
  let s_ctx = varctx_var_shift ctx.var_ctx in
  {
    var_ctx   = BBranch (s_ctx, BLeaf (n_var, bi), p) ;
    tyvar_ctx = ctx.tyvar_ctx ;
    cs_ctx    = ctx.cs_ctx ;
  }
let extend_var2 id1 bi1 id2 bi2 ?(p=PConst 1.0) ?(q=PConst 1.0) ctx =
  let n_var1 = {
    v_name  = id1;
    v_type  = BiVar;
    v_index = PRight (PLeft PHere);
    v_size  = (Bunch.length ctx.var_ctx) + 2;
  } in
  let n_var2 = {
    v_name  = id2;
    v_type  = BiVar;
    v_index = PRight (PRight PHere);
    v_size  = (Bunch.length ctx.var_ctx) + 2;
  } in
  let s_ctx = varctx_var_shift ctx.var_ctx in
  {
    var_ctx   = BBranch (s_ctx, BBranch (BLeaf (n_var1, bi1), BLeaf (n_var2, bi2), q), p) ;
    tyvar_ctx = ctx.tyvar_ctx ;
    cs_ctx    = ctx.cs_ctx ;
  }

(* Extend the context with a new binding. Return the new variable and
   the new context. *)
let extend_ty_var id bi ctx =
  let n_var = {
    v_name = id;
    v_type = BiTyVar;
    v_index = 0;
    v_size = (length ctx.tyvar_ctx) + 1;
  } in
  let s_tmctx = varctx_ty_shift 0 1 ctx.var_ctx   in
  let s_tyctx = tyvar_ctx_shift 0 1 ctx.tyvar_ctx in
  let s_csctx = cs_ctx_shift    0 1 ctx.cs_ctx    in
  {
    var_ctx   = s_tmctx ;
    tyvar_ctx = (n_var, bi) :: s_tyctx ;
    cs_ctx    = s_csctx ;
  }

(* Extend the size constraint store with a new equation *)
let extend_cs cs ctx =
  { ctx with cs_ctx = cs :: ctx.cs_ctx }

(* let remove_first_var ctx =
  if ctx.var_ctx = [] then
  (* We got no generated context, we are coming from a constant term *)
    ctx
  else
    let s_ctx = varctx_var_shift 0 (-1) ctx.var_ctx in
    {
      var_ctx   = List.tl s_ctx;
      tyvar_ctx = ctx.tyvar_ctx ;
      cs_ctx    = ctx.cs_ctx ;
    } *)

let remove_first_ty_var ctx =
  if ctx.tyvar_ctx = [] then
  (* We got no generated context, we are coming from a constant term *)
    ctx
  else
    let s_tyctx = tyvar_ctx_shift 0 (-1) ctx.tyvar_ctx in
    let s_tmctx = varctx_ty_shift 0 (-1) ctx.var_ctx   in
    let s_csctx = cs_ctx_shift    0 (-1) ctx.cs_ctx    in
    {
      var_ctx   = s_tmctx ;
      tyvar_ctx = List.tl s_tyctx ;
      cs_ctx    = s_csctx ;
    }

(* Accessing to the variable in the context *)
let access_var ctx idx = Option.get @@ Bunch.index ctx.var_ctx idx
let access_ty_var ctx i = List.nth ctx.tyvar_ctx i
