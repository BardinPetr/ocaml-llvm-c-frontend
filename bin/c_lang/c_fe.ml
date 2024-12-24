open C_syntax
open Llvm

let llc = Llvm.global_context ()
let llb = builder llc
let llm = create_module llc "main"

module StrMap = Map.Make (String)

type m_var = lltype * llvalue StrMap.t

let uie () = failwith "not implemented"

let tr_literal = function
  | IntLit i -> const_int (i32_type llc) i
  | FloatLit f -> const_float (float_type llc) f
  | CharLit c -> const_int (i8_type llc) (Char.code c)
  | StringLit s -> const_stringz llc s

let rec tr_type = function
  | TVoid -> void_type llc
  | TChar -> i8_type llc
  | TShort -> i16_type llc
  | TInt -> i32_type llc
  | TLong -> i64_type llc
  | TFloat -> float_type llc
  | TDouble -> double_type llc
  | TPtr _ | TStruct _ -> pointer_type llc
  | TArray (base, dim) -> vector_type (tr_type base) dim
  | TEllipsis -> failwith "invalid use of ..."

let rec tr_expression locals = function
  | ExLiteral l -> tr_literal l
  | ExBOp (op, e1, e2) ->
      let exec =
        match op with
        | OpAdd -> build_add
        | OpSub -> build_sub
        | OpMul -> build_mul
        | OpDiv -> build_sdiv
        | OpMod -> build_srem
        | OpAnd -> build_and
        | OpXor -> build_xor
        | OpOr -> build_or
        | OpLAnd -> build_and
        | OpLOr -> build_or
        | OpRight -> build_ashr
        | OpLeft -> build_shl
        | OpCmpLe -> build_icmp Icmp.Sle
        | OpCmpLt -> build_icmp Icmp.Slt
        | OpCmpGe -> build_icmp Icmp.Sge
        | OpCmpGt -> build_icmp Icmp.Sgt
        | OpCmpEq -> build_icmp Icmp.Eq
        | OpCmpNe -> build_icmp Icmp.Ne
      in
      exec (tr_expression locals e1) (tr_expression locals e2) "tmp" llb
  | ExUOpPre (op, e) ->
      let tr_e = tr_expression locals e in
      let exec =
        match op with
        | UOMinus -> build_neg tr_e
        | UOInv -> build_not tr_e
        | UONeg -> uie ()
        | UOInc -> uie ()
        | UODec -> uie ()
        | UODeref -> uie ()
        | UORef -> uie ()
      in
      exec "tmp" llb
  | ExUOpPost (op, e) ->
      let tr_e = tr_expression locals e in
      let exec =
        match op with UOInc -> uie () | UODec -> uie () | _ -> uie ()
      in
      exec "tmp" llb
  | ExId n ->
      let typ, var = StrMap.find n locals in
      build_load typ var "tmp" llb
  | _ -> uie ()
(*   
  | ExAOp asn_op, expr, expr -> 
  | ExCast c_type, expr -> 
  | ExSizeof c_type -> 
  | ExTernary expr, expr, expr -> 
  | ExArrIdx expr, expr -> 
  | ExAccess expr, string -> 
  | ExPAccess expr, string -> 
  *)

let rec tr_statement locals = function
  | StExpr expr ->
      tr_expression locals expr |> ignore;
      locals
  | StDecl (DeclVar (typ, name, init)) ->
      let typ = tr_type (TPtr typ) in
      let var = build_alloca typ name llb in
      (match init with
      | Some e -> build_store var (tr_expression locals e) llb |> ignore
      | _ -> ());
      StrMap.add name (typ, var) locals
  | StReturn None ->
      build_ret_void llb |> ignore;
      locals
  | StReturn (Some e) ->
      build_ret (tr_expression locals e) llb |> ignore;
      locals
  | StBlock lst -> List.fold_left (fun l i -> tr_statement l i) locals lst
  | _ -> failwith "not implemented"
(*
  | StIf expr * stmt * stmt option -> 
  | StFor expr option * expr option * expr option * stmt -> 
  | StWhile expr * stmt -> 
  | StBreak -> 
  | StContinue -> 
  *)

let tr_function_proto typ_ret typ_args =
  let is_va =
    match List.rev typ_args with TEllipsis :: _ -> true | _ -> false
  in
  (if is_va then
     var_arg_function_type
   else
     function_type)
    (tr_type typ_ret)
    (typ_args
    |> List.filteri (fun i _ -> i != List.length typ_args - 1 || not is_va)
    |> List.map tr_type
    |> Array.of_list)

let tr_function = function
  | Prog decls ->
      decls
      |> List.iter (function
           | FuncGDecl (typ_ret, name, args, body) -> (
               let f_typ =
                 tr_function_proto typ_ret (List.map (fun (i, _) -> i) args)
               in
               let f =
                 if Option.is_none body then
                   declare_function name f_typ llm
                 else
                   define_function name f_typ llm
               in
               match body with
               | Some x ->
                   position_at_end (entry_block f) llb;
                   tr_statement StrMap.empty x |> ignore
               | _ -> ())
           | _ -> ())

let translate (ast : program) =
  tr_function ast;
  let v = Llvm_analysis.verify_module llm in
  if Option.is_some v then print_endline (Option.get v);
  dump_module llm
