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
