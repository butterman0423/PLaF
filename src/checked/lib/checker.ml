(**
* Name: Nathaniel Escaro
* Pledge: I pledge my honor that I have abided by the Stevens Honor System
 **)

open ReM
open Dst
open Parser_plaf.Ast
open Parser_plaf.Parser
       
let rec chk_expr : expr -> texpr tea_result = function 
  | Int _n -> return IntType
  | Var id -> apply_tenv id
  | IsZero(e) ->
    chk_expr e >>= fun t ->
    if t=IntType
    then return BoolType
    else error "isZero: expected argument of type int"
  | Add(e1,e2) | Sub(e1,e2) | Mul(e1,e2)| Div(e1,e2) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    if (t1=IntType && t2=IntType)
    then return IntType
    else error "arith: arguments must be ints"
  | ITE(e1,e2,e3) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    chk_expr e3 >>= fun t3 ->
    if (t1=BoolType && t2=t3)
    then return t2
    else error "ITE: condition not boolean or types of then and else do not match"
  | Let(id,e,body) ->
    chk_expr e >>= fun t ->
    extend_tenv id t >>+
    chk_expr body
  | Proc(var,Some t1,e) ->
    extend_tenv var t1 >>+
    chk_expr e >>= fun t2 ->
    return @@ FuncType(t1,t2)
  | Proc(_var,None,_e) ->
    error "proc: type declaration missing"
  | App(e1,e2) ->
    chk_expr e1 >>=
    pair_of_funcType "app: " >>= fun (t1,t2) ->
    chk_expr e2 >>= fun t3 ->
    if t1=t3
    then return t2
    else error "app: type of argument incorrect"
  | Letrec([(_id,_param,None,_,_body)],_target) | Letrec([(_id,_param,_,None,_body)],_target) ->
    error "letrec: type declaration missing"
  | Letrec([(id,param,Some tParam,Some tRes,body)],target) ->
    extend_tenv id (FuncType(tParam,tRes)) >>+
    (extend_tenv param tParam >>+
     chk_expr body >>= fun t ->
     if t=tRes 
     then chk_expr target
     else error
         "LetRec: Type of recursive function does not match
declaration")

  (* TASK 5.1 *)
  | NewRef(e) -> 
    chk_expr e >>= fun t ->
    return (RefType t)
  | DeRef(e) -> 
    chk_expr e >>= fun t ->
    (match t with
     | RefType t2 -> return t2
     | _ -> error "deref: Expected a reference type"
    )
  | SetRef(e1, e2) ->
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    (match t1 with
     | RefType rt when rt=t2 -> return UnitType
     | RefType rt when rt<>t2 -> error "setref: Value to set does not match reference type"
     | _ -> error "setref: Expected a reference type"
    )
  | BeginEnd([]) ->
    return UnitType
  | BeginEnd(es) ->
    chk_expr (List.hd (List.rev es)) >>= fun t -> 
    return t

  (* TASK 5.2 *)
  | EmptyList(t) ->
    (match t with
     | Some lt -> return (ListType lt)
     | _ -> error "emptylist: Expected a type" 
    )
  | Cons(e1, e2) -> 
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    (match t2 with
     | ListType lt when lt=t1 -> return t2
     | ListType lt when lt<>t1 -> error "cons: type of head and tail do not match"
     | _ -> error "cons: Expected a list type"
    )
  | IsEmpty(e) -> 
    chk_expr e >>= fun t ->
    (match t with
     | ListType _ -> return BoolType
     | TreeType _ -> return BoolType    (* PART OF TASK 5.3 *)
     | _ -> error "empty?: Expected a list or tree type"
    )
  | Hd(e) -> 
    chk_expr e >>= fun t ->
    (match t with
     | ListType lt -> return lt
     | _ -> error "hd: Expected a list type"
    )
  | Tl(e) -> 
    chk_expr e >>= fun t ->
    (match t with
     | ListType _ -> return t
     | _ -> error "tl: Expected a list type"
    )

  (* TASK 5.3 *)
  | EmptyTree(t) ->
    (match t with
     | Some tt -> return (TreeType tt)
     | _ -> error "emptytree: Expected a type"
    ) 
  | Node(de, le, re) -> 
    chk_expr de >>= fun t1 ->
    chk_expr le >>= fun t2 ->
    chk_expr re >>= fun t3 ->
    (match t2, t3 with
     | TreeType lt1, TreeType lt2 when lt1=t1 && lt2=t1 -> return (TreeType t1)
     | TreeType lt1, TreeType lt2 when lt1<>t1 && lt2=t1 -> 
       error "node: Value type does not match left node"
     | TreeType lt1, TreeType lt2 when lt1=t1 && lt2<>t1 -> 
       error "node: Value type does not match right node"
     | TreeType _, _ -> error "node: Expected tree type for right node"
     | _, _ -> error "node: Expected tree type for left node"
    )
  | CaseT(target, emptycase, id1, id2, id3, nodecase) ->
    chk_expr target >>= fun t1 ->
    chk_expr emptycase >>= fun et ->
    (match t1 with
     | TreeType t -> 
       (extend_tenv id1 t >>+ 
        extend_tenv id2 t1 >>+
        extend_tenv id3 t1 >>+
        chk_expr nodecase >>= fun nt ->
        if nt=et
        then return nt
        else error "caseT: Return types of node case and empty case do not match"
       )
     | _ -> error "caseT: Expected a tree type"
    )

  | Debug(_e) ->
    string_of_tenv >>= fun str ->
    print_endline str;
    error "Debug: reached breakpoint"
  | _ -> failwith "chk_expr: implement"    
and
  chk_prog (AProg(_,e)) =
  chk_expr e

(* Type-check an expression *)
let chk (e:string) : texpr result =
  let c = e |> parse |> chk_prog
  in run_teac c

let chkpp (e:string) : string result =
  let c = e |> parse |> chk_prog
  in run_teac (c >>= fun t -> return @@ string_of_texpr t)



