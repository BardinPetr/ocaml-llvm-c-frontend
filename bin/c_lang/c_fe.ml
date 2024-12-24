open C_syntax
open Llvm

let llc = Llvm.global_context ()
let llb = builder llc
let llm = create_module llc "main"

module StrMap = Map.Make (String)

type m_var = llvalue StrMap.t
type m_fun = lltype StrMap.t

let tr_literal = function
  | IntLit i -> const_int (i64_type llc) i
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

(* let rec tr_expression = function
  | ExLiteral literal -> 
  | ExId string -> 
  | ExUOpPre unary_op, expr -> 
  | ExUOpPost unary_op, expr -> 
  | ExBOp binary_op, expr, expr -> 
  | ExAOp asn_op, expr, expr -> 
  | ExCast c_type, expr -> 
  | ExSizeof c_type -> 
  | ExTernary expr, expr, expr -> 
  | ExArrIdx expr, expr -> 
  | ExAccess expr, string -> 
  | ExPAccess expr, string -> 
  | ExCall string, expr list ->  *)

let rec tr_statement = function
  | StReturn None -> build_ret_void llb |> ignore
  (* | StReturn Some e -> build_ret_void llb |> ignore *)
  (*
  | StExpr expr -> 
  | StIf expr * stmt * stmt option -> 
  | StFor expr option * expr option * expr option * stmt -> 
  | StWhile expr * stmt -> 
  | StBreak -> 
  | StContinue -> 
  | StDecl var_decl -> 
  *)
  | StBlock lst -> List.iter tr_statement lst
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
                   tr_statement x
               | _ -> ())
           | _ -> ())

let translate (ast : program) =
  tr_function ast;
  (* (match ast with
  | Prog decls ->
      decls
      |> List.iter (function FuncGDecl (typ_ret, name, args, body) ->
             body
             |> Option.get
             |> List.iter (function StExpr e -> ignore (translate_expression e)))); *)
  dump_module llm
