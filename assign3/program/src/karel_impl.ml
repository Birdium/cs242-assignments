open Core
open Option.Monad_infix

exception Unimplemented

(* Set this to true to print out intermediate state between Karel steps *)
let debug = false

type cell =
  | Empty
  | Wall
  | Beeper

type grid = cell list list

type dir =
  | North
  | West
  | South
  | East

type pos = int * int

type state = {
  karel_pos : pos;
  karel_dir : dir;
  grid : grid;
}

let get_cell (grid : grid) ((i, j) : pos) : cell option =
  (List.nth grid j) >>= fun l -> List.nth l i
;;

let set_cell (grid : grid) ((i, j) : pos) (cell : cell) : grid =
  List.mapi grid ~f:(fun j' l ->
    if j = j' then List.mapi l ~f:(fun i' c -> if i = i' then cell else c)
    else l)
;;

let state_to_string (state : state) : string =
  (* raise Unimplemented *)
  let chgrid = 
    List.mapi state.grid ~f:(fun (i : int) (lst : cell list) : string list -> 
      List.mapi lst ~f:(fun (j : int) (c : cell) : string -> 
        if (i, j) = state.karel_pos then 
          "K"
        else 
          match c with
          | Empty -> "."
          | Wall -> "x" 
          | Beeper -> "B"))
  in 
  let slist = List.map chgrid ~f:(fun (row : string list) -> String.concat ~sep:" " row) in 
  String.concat ~sep:"\n" slist
;;

let empty_grid (m : int) (n : int) : grid =
  List.map (List.range 0 m) ~f:(fun _ ->
    List.map (List.range 0 n) ~f:(fun _ -> Empty))
;;

type predicate =
  | FrontIs of cell
  | NoBeepersPresent
  | Facing of dir
  | Not of predicate

type instruction =
  | Move
  | TurnLeft
  | PickBeeper
  | PutBeeper
  | While of predicate * instruction list
  | If of predicate * instruction list * instruction list

let rec predicate_to_string (pred : predicate) : string =
  match pred with
  | FrontIs c ->
    let cellstr = match c with
      | Empty -> "Empty" | Beeper -> "Beeper" | Wall -> "Wall"
    in
    Printf.sprintf "FrontIs(%s)" cellstr
  | NoBeepersPresent -> "NoBeepersPresent"
  | Facing dir ->
    let dirstr = match dir with
      | North -> "North" | South -> "South" | East -> "East" | West -> "West"
    in
    Printf.sprintf "Facing(%s)" dirstr
  | Not pred' -> Printf.sprintf "Not(%s)" (predicate_to_string pred')

let rec instruction_to_string (instr : instruction) : string =
  match instr with
  | Move -> "Move"
  | TurnLeft -> "TurnLeft"
  | PickBeeper -> "PickBeeper"
  | PutBeeper -> "PutBeeper"
  | While (pred, instrs) ->
    Printf.sprintf "While(%s, [%s])"
      (predicate_to_string pred)
      (instruction_list_to_string instrs)
  | If (pred, then_, else_) ->
    Printf.sprintf "If(%s, [%s], [%s])"
      (predicate_to_string pred)
      (instruction_list_to_string then_)
      (instruction_list_to_string else_)
and instruction_list_to_string (instrs: instruction list) : string =
  String.concat ~sep:", " (List.map ~f:instruction_to_string instrs)


let rec eval_pred (state : state) (pred : predicate) : bool =
  (* raise Unimplemented *)
  match pred with
  | FrontIs cell -> 
    let (col, row) = state.karel_pos in 
    let (ncol, nrow) = 
      (match state.karel_dir with 
      | North -> (col, row - 1)
      | South -> (col, row + 1)
      | West -> (col - 1, row)
      | East -> (col + 1, row))
    in
    (match get_cell state.grid (ncol, nrow) with
    | None -> true
    | Some x -> x = cell)  
  | Facing dir -> state.karel_dir = dir
  | Not pred' -> not (eval_pred state pred')
  | NoBeepersPresent -> get_cell state.grid state.karel_pos <> Some Beeper


let rec step (state : state) (code : instruction) : state =
  (* raise Unimplemented *)
  match code with
  | Move -> 
    let (col, row) = state.karel_pos in
    let (ncol, nrow) = 
      (match state.karel_dir with 
      | North -> (col, row - 1)
      | South -> (col, row + 1)
      | West -> (col - 1, row)
      | East -> (col + 1, row))
    in
    (match get_cell state.grid (ncol, nrow) with
    | None -> state
    | Some x -> 
      if x = Wall then 
        state
      else 
        {state with karel_pos = (ncol, nrow)})

  | TurnLeft -> 
    (match state.karel_dir with
    | North -> {state with karel_dir = West}
    | West -> {state with karel_dir = South}
    | South -> {state with karel_dir = East}
    | East -> {state with karel_dir = North})
  | PickBeeper -> 
    let (col, row) = state.karel_pos in
    (match get_cell state.grid (col, row) with
    | Some Beeper -> {state with grid = set_cell state.grid (col, row) Empty} 
    | _ -> state
    )
  | PutBeeper ->
  let (col, row) = state.karel_pos in
  (match get_cell state.grid (col, row) with
  | Some Empty -> {state with grid = set_cell state.grid (col, row) Beeper} 
  | _ -> state
  )
  | While (pred, instrs) -> 
    if (eval_pred state pred) then 
      step_list state (instrs @ [While (pred, instrs)])
    else
      state
  | If (pred, then_, else_) ->
    if (eval_pred state pred) then 
      step_list state then_
    else 
      step_list state else_ 



and step_list (state : state) (instrs : instruction list) : state =
  List.fold instrs ~init:state ~f:(fun state instr ->
    if debug then
       (Printf.printf "Executing instruction %s...\n"
          (instruction_to_string instr);
        let state' = step state instr in
        Printf.printf "Executed instruction %s. New state:\n%s\n"
          (instruction_to_string instr)
          (state_to_string state');
        state')
     else
       step state instr)

;;

let checkers_algo : instruction list = [
While ((Not (FrontIs Wall)) , [
While ((Not (FrontIs Wall)) ,
[
 PutBeeper; Move; Move;	
]
);

TurnLeft; TurnLeft;
Move;
If ((NoBeepersPresent), [ TurnLeft; TurnLeft; Move; PutBeeper], [TurnLeft; TurnLeft; Move]);
TurnLeft; TurnLeft; TurnLeft;

If ((Not (FrontIs Wall)),
[
Move;
TurnLeft; TurnLeft; TurnLeft;

While ((Not (FrontIs Wall)) ,
[
 Move;	
]
);

TurnLeft; TurnLeft;

While ((Not (FrontIs Wall)) ,
[
 Move; PutBeeper; Move;	
]
);

TurnLeft; TurnLeft;
Move;
If ((NoBeepersPresent), [ TurnLeft; TurnLeft; Move; PutBeeper], [TurnLeft; TurnLeft; Move]);
TurnLeft; TurnLeft; TurnLeft;

If ((Not (FrontIs Wall)), 
[
Move;
TurnLeft; TurnLeft; TurnLeft;

While ((Not (FrontIs Wall)) ,
[
 Move;	
]
);

TurnLeft; TurnLeft;
], []);

], 
[]);

]);

If ((Facing East), [TurnLeft; TurnLeft] , [If ((Facing North), [TurnLeft], [ If ((Facing South), [TurnLeft; TurnLeft; TurnLeft], [])]) ]);
While ((Not (FrontIs Wall)) ,
[
 Move;	
]
);
]
