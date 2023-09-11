open Core

exception Unimplemented

type ngram = string list
type ngram_map = (ngram, string list) Map.Poly.t
type word_distribution = float String.Map.t

let rec remove_last_impl1 (l : string list) : string list =
  match l with 
  | [] -> []
  | x :: [] -> []
  | x :: l' -> x :: remove_last_impl1 l' 
;;

assert (remove_last_impl1 ["a"; "b"] = ["a"]);
assert (remove_last_impl1 ["a"] = []);
assert (remove_last_impl1 [] = []);
assert (remove_last_impl1 ["1"; "1"; "4"; "5"; "1"; "4"] = ["1"; "1"; "4"; "5"; "1"]);
;;

let remove_last_impl2 (l : string list) : string list =
  List.filteri l ~f:(fun (i : int) (n : string) : bool -> (i <> List.length l - 1))
;;

assert (remove_last_impl2 ["a"; "b"] = ["a"]);
assert (remove_last_impl2 ["a"] = []);
assert (remove_last_impl2 [] = []);
assert (remove_last_impl2 ["1"; "1"; "4"; "5"; "1"; "4"] = ["1"; "1"; "4"; "5"; "1"]);
;;

let compute_ngrams (l : string list) (n : int) : string list list =
  let len = List.length l in
  let rec ngram (l : string list) (n: int) : string list = 
    let len = List.length l in
    match l with 
    | [] -> []
    | _ when n <= 0 -> []
    | sublist when List.length sublist < n -> []
    | x :: sublist -> x :: ngram sublist (n - 1)
  in 
  let (ans, _) = List.fold_left l ~init:([], l) ~f:(fun (accum, l') s -> 
    let ng = ngram l' n in
    if List.length ng < n then 
      (accum, l')
    else 
      match List.tl l' with 
      | None -> (ng :: accum, [])
      | Some t1 -> (ng :: accum, t1))
  in
  List.rev ans
;;

compute_ngrams ["a"; "b"; "c"] 2;;

assert (compute_ngrams ["a"; "b"; "c"] 2 = [["a"; "b"]; ["b"; "c"]]);
;;

let ngram_to_string ng =
  Printf.sprintf "[%s]" (String.concat ~sep:", " ng)
;;

let ngram_map_new () : ngram_map =
  Map.Poly.empty
;;

let ngram_map_add (map : ngram_map) (ngram : ngram) : ngram_map =
  (* raise Unimplemented *)
  let len = List.length ngram in
  let (k, d) = List.split_n ngram (len - 1) in 
  match Map.Poly.find map k with 
  | None -> Map.Poly.set map ~key:k ~data:d
  | Some x -> Map.Poly.set map ~key:k ~data:(x @ d)
;;

let () =
  let map = ngram_map_new () in
  let map = ngram_map_add map ["a"; "b"] in
  (* Add your own tests here! *)
  ()
;;

let ngram_map_distribution (map : ngram_map) (ngram : ngram)
  : word_distribution option =
  (* raise Unimplemented *)
  let v = 
    match Map.Poly.find map ngram with 
    | None -> []
    | Some x -> x
  in
  if v = [] then 
    None
  else
    let len : int = List.length v in
    let nlist = List.map ~f:(fun (s: string) -> (s, ((float_of_int 1) /. (float_of_int len)))) v in
    let prob = String.Map.of_alist_reduce nlist (+.) in
    Some(prob)
;;

let distribution_to_string (dist : word_distribution) : string =
  Sexp.to_string_hum (String.Map.sexp_of_t Float.sexp_of_t dist)
;;

let sample_distribution (dist : word_distribution) : string =
  (* raise Unimplemented *)
  let rnd = Random.float 1.0 in 
  let (r, rs) = String.Map.fold dist ~init:(rnd, "") ~f:(fun ~key ~data (r, outs) -> 
    if r < data then (1. , key)
    else (r -. data, outs))
  in
  rs
;;

let () =
  let map = ngram_map_new () in
  let map = ngram_map_add map ["a"; "b"] in
  let map = ngram_map_add map ["A"; "B"; "C"] in
  let map = ngram_map_add map ["A"; "B"; "C"] in
  let map = ngram_map_add map ["A"; "B"; "D"] in
  let map = ngram_map_add map ["A"; "B"; "Vivek"] in
    let v = match Map.Poly.find map ["A"; "B"] with
		  | None -> []
		  | Some x -> x
	in
  let dist = ngram_map_distribution map ["A"; "B"] in
  match dist with
  | None -> ()
  | Some x -> 
  let sdist = sample_distribution x in
  (*Printf.printf "%s" sdist;
  Printf.printf "%s" (distribution_to_string x);
	Printf.printf "%s" (ngram_to_string v);*)
	()
;;

let rec sample_n (map : ngram_map) (ng : ngram) (n : int) : string list =
  (* raise Unimplemented *)
  if n = 0 then []
  else match Map.Poly.find map ng with
  | None -> []
  | Some x -> 
    let dist = (ngram_map_distribution map ng) in 
    match dist with
    | None -> []
    | Some x -> let s = sample_distribution x in s :: sample_n map ((remove_last_impl2 (List.rev ng)) @ [s]) (n - 1)
;;
