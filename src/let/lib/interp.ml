(**
 * Name: Nathaniel Escaro
 * Pledge: I pledge my honor that I have abided by the Stevens Honor System.
 **)

(** TODOs 
 * Lists
 * TODO emptylist
 *  - creates an empty list
 * TODO cons e1 e2
 *  - adds an e1 to a list
 *  - if e2 is not a list ERRORS
 * TODO hd e1
 *  - Returns the head of e1
 *  - if e1 is not a list ERRORS
 * TODO tl
 *  - Returns the tail of e1
 *  - if e1 is not a list ERRORS
 * TODO empty? e1
 *  - Checks if the list is empty or not
 *  - if e1 is not a list ERRORS
 *
 * Tuple
 * TODO <e1, ..., en>
 *  - Creates a tuple with the values of each of the ei
 * TODO let <id1, ..., idn> = e1 in e2
 *  - Evaluates e1:
 *    - Is tuple
 *    - Extracts each n component of tuple
 *    - Binds ei to idi
 *  - Evaluates e2 with that environment
 **)

open Parser_plaf.Ast
open Parser_plaf.Parser
open Ds

let list_of_expval : exp_val -> (exp_val list) ea_result = function
    | ListVal l -> return l
    | _ -> error "Expected a list!"

(** [eval_expr e] evaluates expression [e] *)
let rec eval_expr : expr -> exp_val ea_result =
  fun e ->
  match e with
  | Int(n) ->
    return (NumVal n)
  | Var(id) ->
    apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1+n2))
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1-n2))
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    return (NumVal (n1*n2))
  | Div(e1,e2) ->
    eval_expr e1 >>=
    int_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    int_of_numVal >>= fun n2 ->
    if n2==0
    then error "Division by zero"
    else return (NumVal (n1/n2))
  | Let(id,def,body) ->
    eval_expr def >>= 
    extend_env id >>+
    eval_expr body 
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b 
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    int_of_numVal >>= fun n ->
    return (BoolVal (n = 0))
  | Pair(e1,e2) ->
    eval_expr e1 >>= fun ev1 ->
    eval_expr e2 >>= fun ev2 ->
    return (PairVal(ev1,ev2))
  | Fst(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun (l,_) ->
    return l
  | Snd(e) ->
    eval_expr e >>=
    pair_of_pairVal >>= fun (_,r) ->
    return r
  | Debug(_e) ->
    string_of_env >>= fun str ->
    print_endline str; 
    error "Debug called"
  
  (** ADDITIONS START HERE **)
 
  | EmptyList(_t) ->
    return (ListVal [])
  | Cons(e1, e2) ->
    eval_expr e2 >>=
    list_of_expval >>= fun lst ->
    eval_expr e1 >>= fun v ->
    return (ListVal (v::lst))
  | Hd(e) -> 
    eval_expr e >>=
    list_of_expval >>= fun lst ->
    return (List.hd lst)
  | Tl(e) -> failwith ""
  | IsEmpty(e) -> failwith ""
  | Tuple(es) -> failwith ""
  | Untuple(ids, e1, e2) -> failwith ""

  (** ADDITIONS END HERE **)

  | _ -> failwith "Not implemented yet!"

(** [eval_prog e] evaluates program [e] *)
let eval_prog (AProg(_,e)) =
  eval_expr e


(** [interp s] parses [s] and then evaluates it *)
let interp (e:string) : exp_val result =
  let c = e |> parse |> eval_prog
  in run c
  


