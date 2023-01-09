(* Copyright (c) 2013, The Trustees of the University of Pennsylvania
   All rights reserved.

   LICENSE: 3-clause BSD style.
   See the LICENSE file for details on licensing.
*)
open Support.FileInfo
open Bunch

(* Different types of variable binding, for debug purposes *)
type fuzz_binding =
    BiVar    (* Regular varible *)
  | BiTyVar  (* Type variable   *)
  | BiETyVar (* Existential variable  *)
[@@deriving show]

(* Variables and binders *)
type 'a var_info = {
  (* Indexes start a 0 *)
  v_index : 'a;

  (* Debug fields *)
  v_name  : string;
  v_type  : fuzz_binding;
  v_size  : int;
} [@@deriving show]

(* Contexts of type 'a *)
type list_var = int var_info [@@deriving show]
type bunch_var = path var_info [@@deriving show]
type 'a list_ctx = (list_var * 'a) list [@@deriving show]
type 'a bunch_ctx = ((bunch_var * 'a), list_var) bunch [@@deriving show]
type p = list_var Bunch.p [@@deriving show]

(* Adds n to a var_info *)
val var_shift : int -> int -> list_var -> list_var
val var_shift_left : bunch_var -> bunch_var
val var_shift_right : bunch_var -> bunch_var

(* All of the fields are debug information *)

type binder_info = {
  b_name : string;
  b_size : int;          (* How many outside binders we had when this binded was found *)
  b_type : fuzz_binding;
  b_prim : bool;
} [@@deriving show]

(* Types *)

(* Part 0: kinds *)
type kind =
    Star
  | Size
  | Sens
  | Space
  [@@deriving show]

(* Sensitivities *)
type si =
  | SiZero
  | SiSucc  of si
  | SiInfty
  | SiConst of float
  | SiVar   of list_var
  | SiAdd   of si * si
  | SiMult  of si * si
  | SiLub   of si * si
  | SiRoot  of p * si
  | SiLp    of si * si * p
  | SiContrFac of p * p
  (* We only allow to sup to happen over the first variable *)
  | SiSup   of binder_info * kind * si
  | SiCase  of si * si * binder_info * si
  [@@deriving show]

(* Shift variable indexes by n *)
val si_shift : int -> int -> si -> si

type si_cs = SiEq of (si * si) [@@deriving show]

val cs_shift : int -> int -> si_cs -> si_cs

(* Number of binders, index, and sens *)
(* Part 2: Regular "HM" types *)
(* Primitive types *)
type ty_prim =
    PrimNum
  | PrimInt
  | PrimUnit
  | PrimBool
  | PrimString
  | PrimClipped
  | PrimDBS
  [@@deriving show]

(* Types with one argument *)
(* XXX: Extend to types with n-ary arguments *)
type ty_prim1 =
    Prim1Set
  | Prim1Bag
  | Prim1Fuzzy
  [@@deriving show]

type ty =
  (* variables used in bindings *)
    TyVar  of list_var

  (* Primitive types *)
  | TyPrim  of ty_prim
  | TyPrim1 of (ty_prim1 * ty)

  (* ADT *)
  | TyUnion     of ty * ty
  | TyTensor    of ty * ty * p

  (* Functional type *)
  | TyLollipop of ty * si * ty * p

  (* Fixpoint type *)
  | TyMu of binder_info * ty

  (* Quantified types *)
  | TyForall of binder_info * kind * ty
  | TyExistsSize of binder_info * ty

  (********************************************************************)
  (* Dependent types *)
  | TySizedNat of si
  | TySizedNum of si
  | TyList     of ty * si
  [@@deriving show]

(* XXX: This is incorrect, right now it shifts all the indexes, thus
   it is buggy, see the other comment *)

(* Shift all the open indexes by n *)
val ty_shift : int -> int -> ty -> ty

(* Substitutions *)

(* Capture avoiding sub, the term must be dependent on the number of
   binders under it is replaced *)
val ty_subst     : int -> ty -> ty -> ty
val ty_si_subst  : int -> si -> ty -> ty

(* Terms *)

(* Primitive Terms *)
type term_prim =
    PrimTUnit
  | PrimTNum    of float
  | PrimTInt    of int
  | PrimTBool   of bool
  | PrimTString of string
  | PrimTFun    of string * ty
  | PrimTDBS    of string
  [@@deriving show]

val type_of_prim : term_prim -> ty

type term =
    TmVar of info * bunch_var

  (*  *)
  | TmPair      of info * term * term * p
  | TmTensDest  of info * binder_info * binder_info * term * term
  (* Remove the annotation *)
  | TmUnionCase of info * term * si * ty           * binder_info * term * binder_info * term
  (*                      t      [si] return ty of { inl(x)     => tm1  | inl(y)     => tm2  } *)

  (* Primitive terms *)
  | TmPrim     of info * term_prim

  (* The three fundamental constructs of our language: *)

  (* Regular Abstraction and Applicacion *)
  | TmApp of info * term * term

  (* In a lambda is possible to independently annotate the input and return type *)
  | TmAbs of info * binder_info * (si * ty * p) * ty option * term

  (* & constructor *)
  | TmAmpersand of info * term * term

  (* Recursive data types *)
  | TmFold    of info * ty * term
  | TmUnfold  of info * term

  (* Only needed to avoid type inference *)
  | TmLet      of info * binder_info * si * p option * term * term
  | TmLetRec   of info * binder_info * ty * term * term
  | TmSample   of info * binder_info * term * term

  (***********************************************************************)
  (* What to do with this, what is the typing rule?                      *)
  | TmTypedef of info * binder_info * ty * term

  (***********************************************************************)
  (* Dependent case expressions, removal pending on type constraints     *)

  (*                      t      return ty of {nil => tm1  | (    x     ::     xs      ) [si]       => tm2 } *)
  | TmListCase  of info * term * ty                 * term * binder_info * binder_info * binder_info * term
  (*                      t      return ty of {Z => tm1  | (S x)         [si]       => tm2 }                 *)
  | TmNatCase   of info * term * ty               * term * binder_info * binder_info * term

  (* Pack/Unpack *)
  | TmPack of info * term * si * ty
  | TmUnpack of info * binder_info * binder_info * term * term

  (* Sensitity Abstraction and Applicacion *)
  | TmTyAbs of info * binder_info * kind * term
  | TmSiApp of info * term * si
  | TmTyApp of info * term * ty
  [@@deriving show]

val tmInfo : term -> info

(* Substitution for type and sens annotations *)
(* tm[t/x] *)
val term_ty_subst : int -> ty -> term -> term
val term_si_subst : int -> si -> term -> term
