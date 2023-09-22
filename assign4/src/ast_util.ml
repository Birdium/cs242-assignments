open Flags
open Core

exception Unimplemented

let fresh s = s ^ "'"

module Type = struct
  open Ast.Type

  let rec substitute_map (rename : t String.Map.t) (tau : t) : t =
    match tau with
    | Num -> Num
    (* Add more cases here! *)
    | Bool -> Bool
    | Unit -> Unit 
    | Var variable ->
      let var_opt = String.Map.find rename variable in
      (match var_opt with
      | Some var -> var
      | None -> tau)
    | Fn {arg; ret} -> Fn {
      arg = substitute_map rename arg;
      ret = substitute_map rename ret;
    }
    | Product {left; right} -> Product {
      left = substitute_map rename left;
      right = substitute_map rename right;
    }
    | Sum {left; right} -> Sum {
      left = substitute_map rename left;
      right = substitute_map rename right;
    }
    | Forall {a; tau} -> 
      let fr = fresh a in 
      let a' = Var(fr) in 
      let nrename = String.Map.set rename ~key:a ~data:a' in 
      Forall {
        a = fr;
        tau = substitute_map nrename tau;
      }

    | _ -> raise Unimplemented

  let substitute (x : string) (tau' : t) (tau : t) : t =
    substitute_map (String.Map.singleton x tau') tau

  let rec to_debruijn (tau : t) : t =
    let rec aux (depth : int String.Map.t) (tau : t) : t =
      let increment_depth map = 
        String.Map.fold 
          ~init:String.Map.empty
          ~f:(fun ~key ~data updated_map -> 
            let updated_value = data + 1 in 
            String.Map.set updated_map ~key:key ~data:updated_value)
          map
      in
      match tau with
      | Num -> Num
      | Bool -> Bool
      | Unit -> Unit 
      | Var variable -> 
        let var_opt = String.Map.find depth variable in 
        (match var_opt with 
        | Some d -> Var(Int.to_string d)
        | None -> tau
        )
      | Fn {arg; ret} -> Fn {
        arg = aux depth arg;
        ret = aux depth ret;
      }
      | Product {left; right} -> Product {
        left = aux depth left;
        right = aux depth right;
      }
      | Sum {left; right} -> Sum {
        left = aux depth left;
        right = aux depth right;
      }
      | Forall {a; tau} -> 
        let idepth = increment_depth depth in 
        let ndepth = String.Map.set idepth ~key:a ~data:0 in 
        Forall {
          a = "_";
          tau = aux ndepth tau
        }
      (* Add more cases here! *)
      (* | Fn x -> Fn x *)
      (* | Var x ->
        try 
          let d = String.Map.find depth x in
          Var (Int.to_string d)
        with
        | Not_found -> Var x 
      | Fn x -> Fn {
        let increment_depth map = 
          String.Map.fold
          ~init:String.Map.empty
          ~f:(fun key value updated_map -> 
            let updated_value = value + 1 in
            String.Map.add updated_map ~key ~data:updated_value) 
          map 
        in  
        let incremented_map = increment_depth depth in 
        String.Map.set increment_depth ~key:x ~data:x.arg
      } *)
      | Unit -> Unit
      | Product {left; right} -> Product {left; right} 
      | Sum {left; right} -> Sum {left; right}
      | _ -> 
        (* Printf.printf "%s\n" (to_string tau);  *)
        raise Unimplemented
    in
    aux String.Map.empty tau

  let rec aequiv (tau1 : t) (tau2 : t) : bool =
    let rec aux (tau1 : t) (tau2 : t) : bool =
      match (tau1, tau2) with
      | (Num, Num) -> true
      | (Bool, Bool) | (Unit, Unit) -> true
      | (Var x, Var y) -> x = y
      | (Fn x, Fn y) -> aux x.arg y.arg && aux x.ret y.ret
      | (Sum x, Sum y) -> aux x.left y.left && aux x.right y.right
      | (Product x, Product y) -> aux x.left y.left && aux x.right y.right
      | (Rec x, Rec y) -> aux x.tau y.tau
      | (Forall x, Forall y) -> aux x.tau y.tau
      | (Exists x, Exists y) -> aux x.tau y.tau
      | _ -> false
    in
    aux (to_debruijn tau1) (to_debruijn tau2)

  let inline_tests () =
    let p = Parser.parse_type_exn in

    assert (
      aequiv
        (substitute "a" (p "num") (p "forall b . a"))
        (p "forall a . num"));
    assert (
      aequiv
        (substitute "a" (p "b") (p "forall b . a"))
        (p "forall c . b"));
    assert (
      not (aequiv
        (substitute "a" (p "b") (p "forall b . a"))
        (p "forall b . b")));
    assert (
      aequiv
        (substitute "a" (p "b") (p "forall b . forall b . a"))
        (p "forall q . forall c . b"));
    assert (
      not (aequiv
        (substitute "a" (p "b") (p "forall b . forall b . a"))
        (p "forall a . forall b . a")));

    assert (aequiv (p "forall a . a") (p "forall b . b"));
    assert (not (aequiv (p "forall a . a") (p "forall b . num")));
    assert (aequiv
              (p "forall a . forall b . a -> b")
              (p "forall x . forall y . x -> y"))

  (* Uncomment the line below when you want to run the inline tests. *)
  (* let () = inline_tests () *)
end

module Expr = struct
  open Ast.Expr

  let rec substitute_map (rename : t String.Map.t) (e : t) : t =
    match e with
    | Num _ -> e
    | Binop {binop; left; right} -> Binop {
      binop;
      left = substitute_map rename left;
      right = substitute_map rename right}
    (* Put more cases here! *)
    | True -> e
    | False -> e
    | If {cond; then_; else_} -> If {
      cond = substitute_map rename cond;
      then_ = substitute_map rename then_;
      else_ = substitute_map rename else_}
    | Relop {relop; left; right} -> Relop {
      relop;
      left = substitute_map rename left;
      right = substitute_map rename right}
    | And {left; right} -> And {
      left = substitute_map rename left;
      right = substitute_map rename right}
    | Or {left; right} -> Or {
      left = substitute_map rename left;
      right = substitute_map rename right}
    | Var variable -> 
      let var_opt = String.Map.find rename variable in
      (match var_opt with
      | Some var -> var
      | None -> e)
    | Lam {x; tau; e} -> 
      let fr = fresh x in
      let x' = Var(fr) in
      let nrename = String.Map.set rename ~key:x ~data:x' in
      let new_e = substitute_map nrename e in
      (* Printf.printf "%s %s\n" fr (to_string new_e); *)
      Lam {x = fr; tau = tau; e = new_e}
    | App {lam; arg} -> App {
      lam = substitute_map rename lam;
      arg = substitute_map rename arg}
    | Unit -> e
    | Pair {left;right} -> Pair {
      left = substitute_map rename left;
      right = substitute_map rename right
    }
    | Project {e;d} -> Project {
      e = substitute_map rename e;
      d = d;
    }
    | Inject {e; d; tau} -> Inject {
      e = substitute_map rename e;
      d = d;
      tau = tau;
    } 
    | Case {e; xleft; eleft; xright; eright} -> 
      let fr_left = fresh xleft in 
      let fr_right = fresh xright in 
      let xleft' = Var(fr_left) in 
      let xright' = Var(fr_right) in 
      let lrename = String.Map.set rename ~key:xleft ~data:xleft' in 
      let rrename = String.Map.set rename ~key:xright ~data:xright' in 
      Case {
        e = substitute_map rename e;
        xleft = fr_left;
        eleft = substitute_map lrename eleft;
        xright = fr_right;
        eright = substitute_map rrename eright;
      }
    | Fix {x; tau; e} -> 
      let fr = fresh x in 
      let x' = Var(fr) in 
      let nrename = String.Map.set rename ~key:x ~data:x' in 
      Fix {
        x = fr; tau = tau; e = substitute_map nrename e;
      }
    | TyLam {a; e} -> 
      let fr = fresh a in
      let a' = Var(fr) in
      let nrename = String.Map.set rename ~key:a ~data:a' in
      let new_e = substitute_map nrename e in
      TyLam {a = fr; e = new_e}
    | TyApp {e; tau} -> TyApp {
      e = substitute_map rename e; tau
    }

    | _ -> raise Unimplemented

  let substitute (x : string) (e' : t) (e : t) : t =
    (* Printf.printf "%s %s %s\n" x (to_string e') (to_string e); *)
    substitute_map (String.Map.singleton x e') e

  let rec to_debruijn (e : t) : t =
    let rec aux (depth : int String.Map.t) (e : t) : t =
      let increment_depth map = 
        String.Map.fold 
          ~init:String.Map.empty
          ~f:(fun ~key ~data updated_map -> 
            let updated_value = data + 1 in 
            String.Map.set updated_map ~key:key ~data:updated_value)
          map
      in
      match e with
      | Num _ -> e
      | Binop {binop; left; right} -> Binop {
        binop; left = aux depth left; right = aux depth right}
        (* Add more cases here! *)
      | True -> e
      | False -> e
      | If {cond; then_; else_} -> If {
        cond = aux depth cond; then_ = aux depth then_; else_ = aux depth else_}
      | Relop {relop; left; right} -> Relop {
        relop; left = aux depth left; right = aux depth right}
      | And {left; right} -> And {
        left = aux depth left; right = aux depth right}
      | Or {left; right} -> Or {
        left = aux depth left; right = aux depth right}
      | Var variable -> 
        let var_opt = String.Map.find depth variable in 
        (match var_opt with
        | Some d -> Var(Int.to_string d)
        | None -> e
        )
      | Lam {x; tau; e} -> 
        let idepth = increment_depth depth in
        let ndepth = String.Map.set idepth ~key:x ~data:0 in
        Lam {
          x = "_";
          tau = tau;
          e = aux ndepth e;
        }
      | App {lam; arg} -> App {
        lam = aux depth lam; arg = aux depth arg;
      } 
      | Unit -> e
      | Pair {left; right} -> Pair {
        left = aux depth left; right = aux depth right;
      }
      | Project {e; d} -> Project {
        e = aux depth e; d;
      }
      | Inject {e; d; tau} -> Inject {
        e = aux depth e; d; tau;
      }
      | Case {e; xleft; eleft; xright; eright} -> 
        let idepth = increment_depth depth in 
        let ldepth = String.Map.set idepth ~key:xleft ~data:0 in 
        let rdepth = String.Map.set idepth ~key:xright ~data:0 in 
        Case {
          e = aux depth e;
          xleft = "_";
          eleft = aux ldepth eleft;
          xright = "_";
          eright = aux rdepth eright;
        }
      | Fix {x; tau; e} -> 
        let idepth = increment_depth depth in
        let ndepth = String.Map.set idepth ~key:x ~data:0 in
        Fix {
          x = "_";
          tau = tau;
          e = aux ndepth e;
        }
      | TyLam {a; e} -> 
        let idepth = increment_depth depth in
        let ndepth = String.Map.set idepth ~key:a ~data:0 in
        TyLam {
          a = "_";
          e = aux ndepth e;
        }
      | TyApp {e; tau} -> TyApp {
        e = aux depth e; tau
      }


      | _ -> raise Unimplemented
    in
    aux String.Map.empty e

  let aequiv (e1 : t) (e2 : t) : bool =
    let rec aux (e1 : t) (e2 : t) : bool =
      match (e1, e2) with
      | (Num n1, Num n2) -> n1 = n2
      | (Var x, Var y) -> x = y
      | (Binop l, Binop r) ->
        l.binop = r.binop && aux l.left r.left && aux l.right r.right
      | (True, True) | (False, False) -> true
      | (If l, If r) ->
        aux l.cond r.cond && aux l.then_ r.then_ && aux l.else_ r.else_
      | (Relop l, Relop r) ->
        l.relop = r.relop && aux l.left r.left && aux l.right r.right
      | (And l, And r) ->
        aux l.left r.left && aux l.right r.right
      | (Or l, Or r) ->
        aux l.left r.left && aux l.right r.right
      | (Lam l, Lam r) ->
        aux l.e r.e
      | (App l, App r) ->
        aux l.lam r.lam && aux l.arg r.arg
      | (Unit, Unit) -> true
      | (Pair l, Pair r) ->
        aux l.left r.left && aux l.right r.right
      | (Project l, Project r) ->
        aux l.e r.e && l.d = r.d
      | (Inject l, Inject r) ->
        aux l.e r.e && l.d = r.d
      | (Case l, Case r) ->
        aux l.e r.e && aux l.eleft r.eleft && aux l.eright r.eright
      | (Fix l, Fix r) -> aux l.e r.e
      | (TyLam l, TyLam r) ->
        aux l.e r.e
      | (TyApp l, TyApp r) -> aux l.e r.e
      | (Fold_ l, Fold_ r) -> aux l.e r.e
      | (Unfold l, Unfold r) -> aux l r
      | (Export l, Export r) -> aux l.e r.e
      | (Import l, Import r) -> aux l.e_mod r.e_mod && aux l.e_body r.e_body
      | _ -> false
    in
    aux (to_debruijn e1) (to_debruijn e2)

  let inline_tests () =
    let p = Parser.parse_expr_exn in
    let t1 = p "(fun (x : num) -> x) y" in
    assert (aequiv (substitute "x" (Num 0) t1) t1);
    assert (aequiv (substitute "y" (Num 0) t1)
              (p "(fun (x : num) -> x) 0"));

    let t2 = p "x + (fun (x : num) -> y)" in
    assert (aequiv
              (substitute "x" (Num 0) t2)
              (p "0 + (fun (x : num) -> y)"));
    assert (aequiv (substitute "y" (Num 0) t2)
              (p "x + (fun (x : num) -> 0)"));

    assert (aequiv (p "fun (x : num) -> x") (p "fun (y : num) -> y"));

    assert (not (aequiv (p "fun (x : num) -> fun (x : num) -> x + x")
                   (p "fun (x : num) -> fun (y : num) -> y + x")));

    assert (
      aequiv
        (p "tyfun a -> fun (x : a) -> x")
        (p "tyfun b -> fun (x : b) -> x"));

    ()

  (* Uncomment the line below when you want to run the inline tests. *)
  (* let () = inline_tests () *)
end
