open C_syntax
open Llvm

let llc = Llvm.global_context ()
let llb = builder llc
let attr_noinline = create_enum_attr llc "noinline" 0L
let attr_optnone = create_enum_attr llc "optnone" 0L

module StrMap = Map.Make (String)

let uie () = failwith "not implemented"

type llv_map = (lltype * llvalue) StrMap.t

type tr_context = {
  llm : llmodule;
  locals : llv_map;
  globals : llv_map;
  func : llvalue;
}

let ctx_update old_ctx nlocals =
  {
    locals = nlocals;
    globals = old_ctx.globals;
    llm = old_ctx.llm;
    func = old_ctx.func;
  }

let i32 i = const_int (i32_type llc) i

let tr_literal = function
  | IntLit i -> i32 i
  | FloatLit f -> const_float (float_type llc) f
  | CharLit c -> const_int (i8_type llc) (Char.code c)
  | StringLit s ->
      let str_data = build_global_stringptr s "" llb in
      build_in_bounds_gep (pointer_type llc) str_data
        [| const_int (i32_type llc) 0 |]
        "strtmp" llb

let rec tr_type = function
  | TVoid -> void_type llc
  | TChar -> i8_type llc
  | TShort -> i16_type llc
  | TInt -> i32_type llc
  | TLong -> i64_type llc
  | TFloat -> float_type llc
  | TDouble -> double_type llc
  | TPtr _ -> pointer_type llc
  | TStruct _ -> uie ()
  | TArray (base, dim) -> vector_type (tr_type base) dim
  | TEllipsis -> failwith "invalid use of ..."

let rec lvalue_of_expression ctx = function
  | ExId n ->
      let typ, var = StrMap.find n ctx.locals in
      (typ, var)
  | ExArrIdx _ | ExUOpPre _ | ExAccess _ | ExPAccess _ | ExCast _ ->
      failwith "not implemented"
  | _ -> failwith "invalid lvalue"

let rec tr_expression ctx = function
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
      exec (tr_expression ctx e1) (tr_expression ctx e2) "tmp" llb
  | ExUOpPre (op, e) -> (
      let tr_e = tr_expression ctx e in
      match op with
      | UOMinus -> build_neg tr_e "tmp" llb
      | UOInv -> build_not tr_e "tmp" llb
      | UOInc -> tr_expression ctx (ExAOp (AsnAdd, e, ExLiteral (IntLit 1)))
      | UODec -> tr_expression ctx (ExAOp (AsnSub, e, ExLiteral (IntLit 1)))
      | UONeg -> uie ()
      | UODeref -> uie ()
      | UORef -> uie ())
  | ExUOpPost (op, e) ->
      let tr_e = tr_expression ctx e in
      let exec =
        match op with UOInc -> uie () | UODec -> uie () | _ -> uie ()
      in
      exec "tmp" llb
  | ExId n ->
      let typ, var = StrMap.find n ctx.locals in
      build_load typ var "tmp" llb
  | ExAOp (op, tgt, src) ->
      let lv_typ, lv_tgt = lvalue_of_expression ctx tgt in
      let src = tr_expression ctx src in
      let src_value =
        match op with
        | AsnBase -> src
        | AsnAdd -> build_add (tr_expression ctx tgt) src "tmp" llb
        | AsnSub -> build_sub (tr_expression ctx tgt) src "tmp" llb
        | _ -> uie ()
      in
      build_store src_value lv_tgt llb |> ignore;
      build_load lv_typ lv_tgt "tmp" llb
  | ExCall (name, e_pars) ->
      let pars =
        List.map (fun i -> tr_expression ctx i) e_pars |> Array.of_list
      in
      let ftyp, f = StrMap.find name ctx.globals in
      let retname =
        if classify_type (return_type ftyp) != TypeKind.Void then
          name ^ "res"
        else
          ""
      in
      build_call ftyp f pars retname llb
  | _ -> uie ()

let rec tr_statement ctx = function
  | StExpr expr ->
      tr_expression ctx expr |> ignore;
      ctx
  | StDecl (DeclVar (typ, name, init)) ->
      let typ = tr_type typ in
      let var = build_alloca typ name llb in
      (match init with
      | Some e -> build_store (tr_expression ctx e) var llb |> ignore
      | _ -> ());
      ctx_update ctx (StrMap.add name (typ, var) ctx.locals)
  | StReturn None ->
      build_ret_void llb |> ignore;
      ctx
  | StReturn (Some e) ->
      build_ret (tr_expression ctx e) llb |> ignore;
      ctx
  | StBlock lst ->
      if List.length lst == 0 then
        ctx
      else
        List.fold_left tr_statement ctx lst
  | StIf (e_input, br_true, br_false) ->
      let blk_start = insertion_block llb in
      let br_false = match br_false with Some x -> x | None -> StBlock [] in
      let e_input = tr_expression ctx e_input in
      let e_input_cast =
        build_sext_or_bitcast e_input (i32_type llc) "ifvalcast" llb
      in
      let cond = build_icmp Icmp.Ne e_input_cast (i32 0) "ifcond" llb in
      (*true*)
      let blk_then = append_block llc "then" ctx.func in
      position_at_end blk_then llb;
      let ctx = tr_statement ctx br_true in
      let blk_then_end = insertion_block llb in
      (*false*)
      let blk_else = append_block llc "else" ctx.func in
      position_at_end blk_else llb;
      let ctx = tr_statement ctx br_false in
      let blk_else_end = insertion_block llb in
      let blk_merge = append_block llc "merge" ctx.func in
      (*check*)
      position_at_end blk_start llb;
      build_cond_br cond blk_then blk_else llb |> ignore;
      (*join*)
      position_at_end blk_then_end llb;
      build_br blk_merge llb |> ignore;
      position_at_end blk_else_end llb;
      build_br blk_merge llb |> ignore;
      position_at_end blk_merge llb;
      ctx
  | StFor (init, check, post, body) ->
      let blk_start = insertion_block llb in

      let blk_check = append_block llc "forcheck" ctx.func in
      let blk_body = append_block llc "forbody" ctx.func in
      let blk_merge = append_block llc "formerge" ctx.func in

      position_at_end blk_start llb;
      tr_statement ctx (StExpr (Option.get init)) |> ignore;
      build_br blk_check llb |> ignore;

      position_at_end blk_check llb;
      let c_check = tr_expression ctx (Option.get check) in
      let c_check =
        build_sext_or_bitcast c_check (i32_type llc) "forvalcast" llb
      in
      let cond = build_icmp Icmp.Ne c_check (i32 0) "chkcond" llb in
      build_cond_br cond blk_body blk_merge llb |> ignore;

      position_at_end blk_body llb;
      let ctx = tr_statement ctx body in
      tr_statement ctx (StExpr (Option.get post)) |> ignore;
      build_br blk_check llb |> ignore;

      position_at_end blk_merge llb;
      ctx
  | _ -> failwith "not implemented"

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

let tr_function_header lv_params ast_params =
  List.fold_left2
    (fun locals (ct, arg_n) arg_lval ->
      let arg_t = tr_type ct in
      let receiver_var = build_alloca arg_t arg_n llb in
      build_store arg_lval receiver_var llb |> ignore;
      StrMap.add arg_n (arg_t, receiver_var) locals)
    StrMap.empty ast_params lv_params

let tr_entry llm = function
  | Prog decls ->
      List.fold_left
        (fun globals d ->
          match d with
          | FuncGDecl (typ_ret, name, args, body) ->
              let f_typ =
                tr_function_proto typ_ret (List.map (fun (i, _) -> i) args)
              in
              let f =
                if Option.is_none body then
                  declare_function name f_typ llm
                else
                  define_function name f_typ llm
              in
              let nglobals = StrMap.add name (f_typ, f) globals in
              (match body with
              | Some x ->
                  add_function_attr f attr_noinline AttrIndex.Function;
                  add_function_attr f attr_optnone AttrIndex.Function;
                  position_at_end (entry_block f) llb;
                  let locals =
                    tr_function_header (Array.to_list (params f)) args
                  in
                  let fctx = { locals; globals = nglobals; llm; func = f } in
                  tr_statement fctx x |> ignore
              | _ -> ());
              nglobals
          | _ -> globals)
        StrMap.empty decls

let translate (ast : program) =
  let llm = create_module llc "main" in
  tr_entry llm ast |> ignore;

  let v = Llvm_analysis.verify_module llm in
  if Option.is_some v then print_endline (Option.get v);

  llm
