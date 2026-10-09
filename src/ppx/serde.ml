(* Codecs for [@spice.serde] variants and polymorphic variants, which encode
   the way Rust's serde derive does (see [Utils.serde]). *)

open Ppxlib
open Parsetree
open Ast_helper
open Utils

type payload =
  | Unit
  | Args of core_type list
  | Record of label_declaration list

type case = {
  (* The name in JSON: the constructor, or its [@spice.as] string. *)
  name : string;
  (* The ReScript constructor. *)
  constructor : string;
  loc : Location.t;
  payload : payload;
  (* The constructor pattern, binding [v0], [v1], ... or the record fields. *)
  lhs : pattern;
  construct : expression option -> expression;
}

let str s = Exp.constant (Pconst_string (s, Location.none, Some "*j"))
let int i = Exp.constant (Pconst_integer (string_of_int i, None))
let arg_names args = List.mapi (fun i _ -> "v" ^ string_of_int i) args

let args_pattern args =
  match args with
  | [] -> None
  | _ ->
      Some
        (arg_names args
        |> List.map (fun name -> Pat.var (mknoloc name))
        |> tuple_or_singleton Pat.tuple)

let name_of_alias ~loc ~has_attr_as ~alias constructor =
  if not has_attr_as then constructor
  else
    match alias.pexp_desc with
    | Pexp_constant (Pconst_string (s, _, _)) -> s
    | _ -> fail loc "@spice.serde needs @spice.as to be a string"

let json_object entries =
  Exp.constraint_
    [%expr JSON.Object (Dict.fromArray [%e Exp.array entries])]
    ctyp_json_t

let validate ~tag cases =
  match tag with
  | None -> ()
  | Some tag ->
      cases
      |> List.iter (fun { loc; payload } ->
             match payload with
             | Unit -> ()
             | Args _ ->
                 fail loc
                   "An internally tagged (@tag) @spice.serde variant needs an \
                    inline record payload, like serde's #[serde(tag = ...)]"
             | Record fields ->
                 if List.exists (fun { pld_name = { txt } } -> txt = tag) fields
                 then fail loc ("A payload field is named like the tag " ^ tag))

let encode_arg generator_settings arg name =
  let encoder, _ = Codecs.generate_value_codecs generator_settings arg in
  Exp.apply (Option.get encoder) [ (Nolabel, make_ident_expr name) ]

let encoder_case generator_settings ~tag { name; payload; lhs } =
  let rhs =
    match (payload, tag) with
    | Unit, None -> [%expr JSON.String [%e str name]]
    | Unit, Some tag ->
        json_object [ [%expr [%e str tag], JSON.String [%e str name]] ]
    | Args args, _ ->
        let value =
          match
            List.map2 (encode_arg generator_settings) args (arg_names args)
          with
          | [ value ] -> value
          | values -> [%expr JSON.Array [%e Exp.array values]]
        in
        json_object [ [%expr [%e str name], [%e value]] ]
    | Record fields, None ->
        json_object
          [
            [%expr
              [%e str name],
                [%e
                  Records.generate_inline_record_encoder_expr generator_settings
                    fields]];
          ]
    | Record fields, Some tag ->
        Records.generate_inline_record_encoder_expr
          ~leading_entries:
            [ [%expr [%e str tag], Some (JSON.String [%e str name])] ]
          generator_settings fields
  in
  Exp.case lhs rhs

let generate_encoder generator_settings ~tag cases =
  let match_expr =
    cases
    |> List.map (encoder_case generator_settings ~tag)
    |> Exp.match_ [%expr v]
  in
  Exp.constraint_ match_expr ctyp_json_t
  |> Exp.fun_ Nolabel None [%pat? v]
  |> expr_func ~arity:1

let with_path prefix expr =
  [%expr
    match [%e expr] with
    | Ok v -> Ok v
    | Error (e : Spice.decodeError) ->
        Error { e with path = [%e str prefix] ^ e.path }]

let decode_args generator_settings { construct } args =
  let decoder arg =
    let _, decoder = Codecs.generate_value_codecs generator_settings arg in
    Option.get decoder
  in
  let names = arg_names args in
  match args with
  | [ arg ] ->
      [%expr
        match [%e decoder arg] payload with
        | Ok v0 -> Ok [%e construct (Some (make_ident_expr "v0"))]
        | Error e -> Error e]
  | _ ->
      let count = List.length args in
      let success =
        Exp.case
          (names
          |> List.map (fun name -> [%pat? Ok [%p Pat.var (mknoloc name)]])
          |> tuple_or_singleton Pat.tuple)
          [%expr
            Ok
              [%e
                construct
                  (Some
                     (names |> List.map make_ident_expr
                     |> tuple_or_singleton Exp.tuple))]]
      in
      let errors = List.mapi (Decode_cases.generate_error_case count) args in
      let decoded =
        args
        |> List.mapi (fun i arg ->
               [%expr [%e decoder arg] (Array.getUnsafe json_arr [%e int i])])
        |> tuple_or_singleton Exp.tuple
      in
      [%expr
        match payload with
        | JSON.Array json_arr ->
            if Array.length json_arr <> [%e int count] then
              Spice.error "Invalid number of arguments to variant constructor"
                payload
            else [%e Exp.match_ decoded (success :: errors)]
        | _ -> Spice.error "Not an array" payload]

let rec decode_by_name cases ~otherwise =
  match cases with
  | [] -> otherwise
  | (name, decoded) :: rest ->
      Exp.ifthenelse
        [%expr spice_tag = [%e str name]]
        decoded
        (Some (decode_by_name rest ~otherwise))

(* [legacy] decodes the default spice encoding, an array with the
   constructor first, given [json_arr]. *)
let generate_decoder generator_settings ~tag ~legacy cases =
  let invalid = [%expr Spice.error "Invalid variant constructor" v] in
  let units =
    cases
    |> List.filter_map (fun { name; payload; construct } ->
           match payload with
           | Unit -> Some (name, [%expr Ok [%e construct None]])
           | _ -> None)
  in
  let object_decoder =
    match tag with
    | None ->
        let with_payload =
          cases
          |> List.filter_map (fun ({ name; constructor; payload } as case) ->
                 let decoded =
                   match payload with
                   | Unit -> None
                   | Args args -> Some (decode_args generator_settings case args)
                   | Record fields ->
                       Some
                         [%expr
                           match payload with
                           | JSON.Object dict ->
                               [%e
                                 Records.generate_inline_record_decoder_expr
                                   generator_settings fields constructor]
                           | _ -> Spice.error "Not an object" payload]
                 in
                 Option.map
                   (fun decoded -> (name, with_path ("." ^ name) decoded))
                   decoded)
        in
        [%expr
          match Dict.toArray dict with
          | [| (spice_tag, payload) |] ->
              [%e decode_by_name with_payload ~otherwise:invalid]
          | _ -> Spice.error "Expected an object with one key" v]
    | Some tag ->
        let records =
          cases
          |> List.filter_map (fun { name; constructor; payload } ->
                 match payload with
                 | Record fields ->
                     Some
                       ( name,
                         Records.generate_inline_record_decoder_expr
                           generator_settings fields constructor )
                 | _ -> None)
        in
        [%expr
          match Dict.get dict [%e str tag] with
          | Some (JSON.String spice_tag) ->
              [%e decode_by_name (units @ records) ~otherwise:invalid]
          | _ -> Spice.error ([%e str tag] ^ " missing") v]
  in
  expr_func ~arity:1
    [%expr
      fun v ->
        match (v : JSON.t) with
        | JSON.String spice_tag -> [%e decode_by_name units ~otherwise:invalid]
        | JSON.Object dict -> [%e object_decoder]
        | JSON.Array [||] -> Spice.error "Expected variant, found empty array" v
        | JSON.Array json_arr -> [%e legacy]
        | _ -> Spice.error "Not a variant" v]

let generate_codecs ({ do_encode; do_decode } as generator_settings) ~tag
    ~legacy cases =
  validate ~tag cases;
  ( (if do_encode then Some (generate_encoder generator_settings ~tag cases)
     else None),
    if do_decode then
      Some (generate_decoder generator_settings ~tag ~legacy:(legacy ()) cases)
    else None )
