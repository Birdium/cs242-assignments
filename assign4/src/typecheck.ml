open Flags
open Core
open Result.Monad_infix
open Ast

exception Unimplemented

let rec typecheck_expr (ctx : Type.t String.Map.t) (e : Expr.t)
  : (Type.t, string) Result.t =
  match e with
  | Expr.Num _ -> Ok Type.Num

  | Expr.Binop {left; right; _} ->
    typecheck_expr ctx left >>= fun tau_left ->
    typecheck_expr ctx right >>= fun tau_right ->
    (match (tau_left, tau_right) with
     | (Type.Num, Type.Num) -> Ok Type.Num
     | _ -> Error (
       Printf.sprintf
         "Binary operands have incompatible types: (%s : %s) and (%s : %s)"
         (Expr.to_string left) (Type.to_string tau_left)
         (Expr.to_string right) (Type.to_string tau_right)))

  (* Add more cases here! *)

  | Expr.True -> Ok Type.Bool
  | Expr.False -> Ok Type.Bool

  | Expr.If {cond; then_; else_} ->
    typecheck_expr ctx cond >>= fun tau_cond -> 
    typecheck_expr ctx then_ >>= fun tau_then ->
    typecheck_expr ctx else_ >>= fun tau_else -> 
    (match (tau_cond, tau_then, tau_else) with 
     | (Type.Bool, tau1, tau2) when tau1 = tau2 -> Ok tau1
     | _ -> Error (
      if tau_cond = Type.Bool then
        Printf.sprintf
          "If condition expression has type: (%s : %s) but was expected of type bool"
          (Expr.to_string cond) (Type.to_string tau_cond)
      else
        Printf.sprintf
          "If operands have incompatible types: (%s : %s) and (%s : %s)"
          (Expr.to_string then_) (Type.to_string tau_then)
          (Expr.to_string else_) (Type.to_string tau_else)
     ))
  
  | Expr.Relop {left; right; _} ->
    typecheck_expr ctx left >>= fun tau_left ->
    typecheck_expr ctx right >>= fun tau_right ->
    (match (tau_left, tau_right) with
     | (Type.Num, Type.Num) -> Ok Type.Bool
     | _ -> Error (
       Printf.sprintf
         "Relop operands have incompatible types: (%s : %s) and (%s : %s)"
         (Expr.to_string left) (Type.to_string tau_left)
         (Expr.to_string right) (Type.to_string tau_right)))
  
  | Expr.And {left; right} ->
    typecheck_expr ctx left >>= fun tau_left ->
    typecheck_expr ctx right >>= fun tau_right ->
    (match (tau_left, tau_right) with
     | (Type.Bool, Type.Bool) -> Ok Type.Bool
     | _ -> Error (
       Printf.sprintf
         "And operands have incompatible types: (%s : %s) and (%s : %s)"
         (Expr.to_string left) (Type.to_string tau_left)
         (Expr.to_string right) (Type.to_string tau_right)))

  | Expr.Or {left; right} ->
    typecheck_expr ctx left >>= fun tau_left ->
    typecheck_expr ctx right >>= fun tau_right ->
    (match (tau_left, tau_right) with
     | (Type.Bool, Type.Bool) -> Ok Type.Bool
     | _ -> Error (
       Printf.sprintf
         "Or operands have incompatible types: (%s : %s) and (%s : %s)"
         (Expr.to_string left) (Type.to_string tau_left)
         (Expr.to_string right) (Type.to_string tau_right)))

  | Expr.Var variable -> (
    try 
      let var_ty = String.Map.find_exn ctx variable in
      Ok var_ty
    with
    | Not_found_s key_not_found -> 
      Error(
        Printf.sprintf
        "Unbounded variable %s" variable)
  )

  | Expr.Lam {x; tau; e} -> 
    let lctx = String.Map.set ctx ~key:x ~data:tau in
    typecheck_expr lctx e >>= fun tau_e -> 
      Ok(Type.Fn{arg = tau; ret = tau_e})
  
  | Expr.App {lam; arg} ->
    typecheck_expr ctx lam >>= fun tau_lam ->
    typecheck_expr ctx arg >>= fun tau_arg ->
    (match tau_lam with
    | Type.Fn {arg = fn_arg; ret = fn_ret} ->
      if Ast_util.Type.aequiv fn_arg tau_arg then
        Ok fn_ret
      else 
        Error(
          Printf.sprintf
          "Incompatible argument types: function: (%s %s), arg: (%s %s)" 
          (Expr.to_string lam) (Type.to_string tau_lam)
          (Expr.to_string arg) (Type.to_string tau_arg)
        )
    | _ -> Error(
      Printf.sprintf
      "(%s, %s) is not a function" (Expr.to_string lam) (Type.to_string tau_lam)
    )
    )
  
  | Expr.Unit -> Ok Type.Unit
  | Expr.Pair {left; right} -> 
    typecheck_expr ctx left >>= fun tau_left -> 
    typecheck_expr ctx right >>= fun tau_right -> 
      Ok (Type.Product {left = tau_left; right = tau_right})
  | Expr.Project {e; d} -> 
    typecheck_expr ctx e >>= fun tau_e -> 
     (match tau_e with
      | Product{left; right} -> 
        (match d with 
        | Left -> Ok left
        | Right -> Ok right
        )
      | _ -> Error(
        Printf.sprintf
        "expression (%s %s) doesn't have product type"
        (Expr.to_string e) (Type.to_string tau_e)
      )
     )
  | Expr.Inject {e; d; tau} -> 
    typecheck_expr ctx e >>= fun tau_e ->
    (match tau with
    | Sum {left; right} when 
      let inj_tau = 
        if d = Left then left else right 
      in 
      Ast_util.Type.aequiv inj_tau tau_e ->   
        Ok tau
    | _ -> Error(
      Printf.sprintf
      "Mismatched sum type in expression (%s: %s)" 
      (Expr.to_string e) (Type.to_string tau_e) ) 
    ) 
  | Expr.Case {e; xleft; eleft; xright; eright} -> 
    typecheck_expr ctx e >>= fun tau_e -> 
    (match tau_e with
    | Sum {left; right} ->
      let lctx = String.Map.set ctx ~key:xleft ~data:left in 
      let rctx = String.Map.set ctx ~key:xright ~data:right in 
      typecheck_expr lctx eleft >>= fun tau_eleft -> 
      typecheck_expr rctx eright >>= fun tau_eright -> 
      if Ast_util.Type.aequiv tau_eleft tau_eright then
        Ok tau_eleft
      else 
        Error(
          Printf.sprintf 
          "Mismatched type in case expression"
        )
    | _ -> Error(
      Printf.sprintf
      "Case expression (%s: %s) should have sum type"
      (Expr.to_string e) (Type.to_string tau_e)) 
    )
  | Expr.Fix {x; tau; e} -> 
    let nctx = String.Map.set ctx ~key:x ~data:tau in 
    typecheck_expr nctx e >>= fun tau_e -> 
    if Ast_util.Type.aequiv tau tau_e then 
      Ok tau
    else
      Error(
        Printf.sprintf
        "TODO"
      )
  | Expr.TyLam {a; e} -> 
    typecheck_expr ctx e >>= fun tau_e -> 
    Ok (Type.Forall {a = a; tau = tau_e})
  | Expr.TyApp {e; tau} -> 
    typecheck_expr ctx e >>= fun tau_e -> 
    let Type.Forall {a = a; tau = tau_body} = tau_e in 
    Ok (Ast_util.Type.substitute a tau tau_body)
  | Expr.Fold_ {e; tau} -> 
    typecheck_expr ctx e >>= fun tau_e -> 
    (match tau with 
    | Type.Rec {a; tau = tau'} ->
      let tau_e' = Ast_util.Type.substitute a tau tau' in 
      if Ast_util.Type.aequiv tau_e tau_e' then Ok (tau)
      else Error(
        Printf.sprintf
        "TODO"
      ) 
    | _ -> Error(
      Printf.sprintf
      "TODO"))
  | Expr.Unfold e -> 
    typecheck_expr ctx e >>= fun tau_e -> 
    (match tau_e with 
    | Type.Rec {a; tau} -> 
      Ok (Ast_util.Type.substitute a tau_e tau)
    | _ -> Error(
      Printf.sprintf
      "TODO"))
  | _ -> 
    (* Printf.sprintf "%s" (Expr.to_string e);  *)
    raise Unimplemented

let typecheck t = typecheck_expr String.Map.empty t

let inline_tests () =
  let p_ex = Parser.parse_expr_exn in
  let p_ty = Parser.parse_type_exn in
  let e1 = p_ex "fun (x : num) -> x" in
  assert (typecheck e1 = Ok(p_ty "num -> num"));

  let e2 = p_ex "fun (x : num) -> y" in
  assert (Result.is_error (typecheck e2));

  let t3 = p_ex "(fun (x : num) -> x) 3"in
  assert (typecheck t3 = Ok(p_ty "num"));

  let t4 = p_ex "((fun (x : num) -> x) 3) 3" in
  assert (Result.is_error (typecheck t4));

  let t5 = p_ex "0 + (fun (x : num) -> x)" in
  assert (Result.is_error (typecheck t5))

(* Uncomment the line below when you want to run the inline tests. *)
(* let () = inline_tests () *)
