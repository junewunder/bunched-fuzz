open Syntax
open Ctx

module FI = Support.FileInfo

(* module P    = Print *)
module Opts = Support.Options
(* open Support.Error *)

(* Not used !
let cs_error        = error_msg Opts.SMT

let cs_warning fi   = message 1 Opts.SMT fi
let cs_info    fi   = message 2 Opts.SMT fi
let cs_info2   fi   = message 3 Opts.SMT fi
let cs_debug   fi   = message 4 Opts.SMT fi
let cs_debug2  fi   = message 5 Opts.SMT fi
let cs_debug3  fi   = message 6 Opts.SMT fi
*)

(* Type of constraints generated by the type-checker when verifying
   subtyping. A constraint has the form

     φ ⇒ R ≤ R'

   where φ is a list of equality constraints (S = S') between size
   expressions, and R and R' are sensitivity expressions. *)

type constr = {
  c_info     : FI.info;

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

(* XXX: constr is redundant with ctx, we should fix that *)
let cs_ctx_shift n d =
  List.map (cs_shift n d)

let tyvar_ctx_shift n d ctx =
  List.map (fun (v, k) -> (var_shift n d v, k)) ctx

let extend_si_var id bi cs =
  let n_var = { (* dvi with *)
    v_name  = id;
    v_type  = BiTyVar;
    v_index = 0;
    v_size  = (List.length cs.c_kind_ctx) + 1;
  } in
  let s_cs    = cs_ctx_shift    0 1 cs.c_cs       in
  let s_ctx   = tyvar_ctx_shift 0 1 cs.c_kind_ctx in
  let s_lower = si_shift        0 1 cs.c_lower    in
  let s_upper = si_shift        0 1 cs.c_upper    in
  { cs with
    c_kind_ctx = (n_var, bi) :: s_ctx ;
    c_cs       = s_cs;
    c_lower    = s_lower;
    c_upper    = s_upper;
  }

let fvar (cs : constr) : si =
  SiVar (fst (List.hd cs.c_kind_ctx))

(* Convenience wrapper around constr *)
let mk_constr_leq (i : FI.info) (ctx : context) (sil : si) (sir : si) : constr =
  { c_info     = i;
    c_kind_ctx = ctx.tyvar_ctx;
    c_cs       = ctx.cs_ctx;
    c_lower    = sil;
    c_upper    = sir;
  }


let cs_store : (constr list) ref = ref []

module Decide = struct

  let is_zero si =
    si = SiZero ||
    si = SiConst 0.0

  let is_infty si =
    si = SiInfty

  (* We should also decide constants, etc... *)
  let decide_leq sil sir =
    if sil = sir then
      Some true
    else if is_zero sil then
      Some true
    else if is_infty sir then
      Some true
    else
      None
end

module Simpl = struct

  let rec si_simpl si = match si with
    | SiAdd (si1, si2)  ->
      let si1' = si_simpl si1 in
      let si2' = si_simpl si2 in
      begin
        match si1', si2' with
	(* | SiConst x, SiConst y -> SiConst (si_add x y) *)
	| _, _                 -> SiAdd (si1', si2')
      end
    | SiMult(si1, si2) ->
      let si1' = si_simpl si1 in
      let si2' = si_simpl si2 in
      begin
        match si1', si2' with
	| SiConst 1.0,   y
	| SiSucc SiZero, y -> y
	| x, SiConst 1.0
	| x, SiSucc SiZero -> x
	| _, _             -> SiMult (si1', si2')
      end
    | SiLub(si1, si2) ->
      let si1' = si_simpl si1 in
      let si2' = si_simpl si2 in
      SiLub(si1', si2')

    | SiCase(si, zsi, b, ssi) ->
      let si'  = si_simpl si  in
      let zsi' = si_simpl zsi in
      let ssi' = si_simpl ssi in
      SiCase(si', zsi', b, ssi')

    | _ -> si
end

module Optimize = struct

  let add_eq (cs : constr) (si1 : si) (si2 : si) : constr =
  { cs with
    c_cs = SiEq (si1, si2) :: cs.c_cs
  }

  let rec is_standard (si : si) : bool =
    match si with
    | SiZero
    | SiInfty
    | SiConst _
    | SiVar   _ -> true
    | SiSucc si -> is_standard si
    | SiAdd  (si1, si2)
    | SiMult (si1, si2) -> is_standard si1 &&
                           is_standard si2
    | SiRoot _
    | SiLub  _
    | SiLp   _
    | SiSup  _
    | SiCase _ -> false

  (* Precondition: sir is standard *)
  let rec leq_simplify (cs : constr) : constr list =
    match cs.c_lower with
    | SiCase(si, zsi, b, ssi) ->
      let z_cs  = {cs with c_lower = zsi}      in
      let z_cs  = add_eq z_cs si SiZero        in
      let z_csl = leq_simplify z_cs            in

      let s_cs  = extend_si_var b.b_name Size cs       in
      (* Note that ssi is already in the extended context *)
      let s_cs  = {s_cs with c_lower = ssi}            in
      (* But si is not *)
      let si'   = si_shift 0 1 si                      in
      let s_cs  = add_eq s_cs si' (SiSucc (fvar s_cs)) in
      let s_csl = leq_simplify s_cs                    in

      z_csl @ s_csl

    | SiLub(si1, si2) ->
      let l_cs  = {cs with c_lower = si1}      in
      let l_csl = leq_simplify l_cs            in
      let r_cs  = {cs with c_lower = si2}      in
      let r_csl = leq_simplify r_cs            in

      l_csl @ r_csl

    | SiAdd (_si1, _si2)
    | SiMult(_si1, _si2) ->
      (* TODO: Do - lower <- si1
                  - lower <- si2,
         then zip all the results with the +/*
      *)
      [cs]

    | _ -> [cs]


  let leq_simplify (cs : constr) : constr list =
    if is_standard cs.c_upper then
      leq_simplify cs
    else
      [cs]

  (* let leq_simplify (cs : constr) : constr list = *)
  (*   [cs] *)
end

(* TODO: log the calls, the decided constraint should get lower
   printing priority *)

(* TODO: This code is not doing any real checking yet *)
let add_cs cs =
  match Decide.decide_leq cs.c_lower cs.c_upper with
  | Some _res-> ()
  | _        -> let cs = Optimize.leq_simplify cs in
                cs_store := cs @ !cs_store

let post_si_leq i (ctx : context) (sil : si) (sir : si) : bool =
  let sil = Simpl.si_simpl sil in
  let sir = Simpl.si_simpl sir in
  add_cs (mk_constr_leq i ctx sil sir) ;
  true

let post_si_eq  i (ctx : context) (sil : si) (sir : si) : bool =
  post_si_leq i ctx sil sir &&
  post_si_leq i ctx sir sil

let get_cs _ = List.rev !cs_store
