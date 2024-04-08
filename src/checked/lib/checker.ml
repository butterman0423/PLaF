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
     | RefType rt with rt=t2 -> return rt
     | RefType rt with rt<>t2 -> error "setref: Value to set does not match reference type"
     | _ -> error "setref: Expected a reference type"
    )
  | BeginEnd([]) ->
    return UnitType
  | BeginEnd(es) -> 
    return @@ chk_expr @@ 
    List.hd @@ (List.rev es)

  (* TASK 5.2 *)
  | EmptyList(t) -> 
    return (ListType t)
  | Cons(e1, e2) -> 
    chk_expr e1 >>= fun t1 ->
    chk_expr e2 >>= fun t2 ->
    (match t2 with
     | ListType lt with lt=t1 -> return lt
     | ListType lt with lt<>t1 -> error "cons: Value to cons does not match list type"
     | _ -> error "cons: Expected a list type"
  | IsEmpty(e) -> 
    chk_expr e >>= fun t ->
    (match t with
     | ListType _ -> return BoolType
     | TreeType _ -> return BoolType    (* PART OF TASK 5.3 *)
     | _ -> error "empty?: Expected a list or tree type"
    if t=ListType
    then return BoolType
    else error "empty?: Expected a list type"
  | Hd(e) -> 
    chk_expr e >>= fun t ->
    (match t with
     | ListType lt -> return lt
     | _ -> error "hd: Expected a list type"
  | Tl(e) -> 
    chk_expr e >>= fun t ->
    if t=ListType
    then return t
    else "tl: Expected a list type"

  (* TASK 5.3 *)
  | EmptyTree(t) -> 
    return (TreeType t)
  | Node(de, le, re) -> 
    chk_expr de >>= fun t1 ->
    chk_expr le >>= fun t2 ->
    chk_expr re >>= fun t3 ->
    (match t2, t3 with
     | TreeType lt1, TreeType lt2 with lt1=t1 && lt2=t1 -> return (TreeType t1)
     | TreeType lt1, TreeType lt2 with lt1<>t1 && lt2=t1 -> 
       error "node: Value type does not match left node"
     | TreeType lt1, TreeType lt2 with lt1=t1 && lt2<>t1 -> 
       error "node: Value type does not match right node"
     | TreeType lt1, _ -> error "node: Expected tree type for right node"
     | _, _ -> error "node: Expected tree type for left node"
  | CaseT(target, emptycase, id1, id2, id3, nodecase) ->
    chk_expr target >>= fun t1 ->
    chk_expr emptycase >>= fun et ->
    (match t1 with
     | TreeType t -> 
       (chk_expr id1 >>= fun d ->
        chk_expr id2 >>= fun ra_lt ->
        chk_expr id3 >>= fun ra_rt ->
        (match ra_lt, ra_rt with
         | lt, rt with d=t && lt=t1 && rt=t2 ->
           ( extend_tenv "d" d >>+
             extend_tenv "lt" lt >>+
             extend_tenv "rt" rt >>+
             chk_expr nodecase >>= fun nt ->
             if nt=et
             then return nt
             else error "caseT: Type of empty case and node case do not match"
           )
         | lt, rt with d=t && lt<>t1 && rt=t2 ->
           error "caseT: Type of lt does not match tree type"
         | lt, rt with d=t && lt=t1 && rt<>t2 ->
           error "caseT: Type of rt does not match tree type"
         | _, _, with d<>t ->
           error "caseT: Type of d does not match tree value type"
        )
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



