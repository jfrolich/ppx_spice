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
  (* [@spice.alias("...")]: other names it decodes from. *)
  aliases : string list;
  (* [@spice.other]: a unit constructor any unknown name decodes to. *)
  other : bool;
  (* [@spice.untagged]: a one-value constructor written as its value alone,
     decoded from what no named constructor reads. *)
  untagged : bool;
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

(* A constructor's JSON name: its [@spice.as], else ReScript's [@as], which
   also renames it in memory, else the constructor. *)
let name_of_attributes ~loc ~has_attr_as ~alias attributes constructor =
  let spice_name = name_of_alias ~loc ~has_attr_as ~alias constructor in
  match get_attribute_by_name attributes "as" with
  | Error s -> fail loc s
  | Ok None -> spice_name
  | Ok (Some attribute) -> (
      match get_expression_from_payload attribute with
      | { pexp_desc = Pexp_constant (Pconst_string (s, _, _)) } ->
          if has_attr_as && s <> spice_name then
            fail loc "@as and @spice.as name the constructor differently"
          else s
      | _ -> fail loc "@spice.serde needs @as to be a string")

(* The constructor attributes serde mode reads besides its name:
   [@spice.alias], [@spice.other] and [@spice.untagged]. *)
let flags_of_attributes ~loc attributes =
  let named name =
    List.filter
      (fun { attr_name = { txt } } -> txt = annotation_name ^ "." ^ name)
      attributes
  in
  let flag name =
    match named name with
    | [] -> false
    | [ { attr_payload = PStr [] } ] -> true
    | [ _ ] -> fail loc ("@spice." ^ name ^ " takes no payload")
    | _ -> fail loc ("Too many occurrences of @spice." ^ name)
  in
  let aliases =
    named "alias"
    |> List.map (fun attribute ->
           match (get_expression_from_payload attribute).pexp_desc with
           | Pexp_constant (Pconst_string (s, _, _)) -> s
           | _ -> fail loc "@spice.alias needs a string")
  in
  (aliases, flag "other", flag "untagged")

(* Fails on serde-only constructor attributes outside serde mode. *)
let reject_flags ~loc attributes =
  match flags_of_attributes ~loc attributes with
  | [], false, false -> ()
  | _ ->
      fail loc
        "@spice.alias, @spice.other and @spice.untagged need @spice.serde"

let json_object entries =
  Exp.constraint_
    [%expr JSON.Object (Dict.fromArray [%e Exp.array entries])]
    ctyp_json_t

let validate_names cases =
  let rec check seen = function
    | [] -> ()
    | (name, loc) :: rest ->
        if List.mem name seen then
          fail loc ("Two constructors are both named " ^ name ^ " in JSON")
        else check (name :: seen) rest
  in
  cases
  |> List.filter (fun { untagged } -> not untagged)
  |> List.concat_map (fun { name; aliases; loc } ->
         List.map (fun name -> (name, loc)) (name :: aliases))
  |> check []

let validate_flags cases =
  let others = List.filter (fun { other } -> other) cases in
  let untagged = List.filter (fun { untagged } -> untagged) cases in
  (match others with
  | _ :: { loc } :: _ -> fail loc "Only one constructor can be @spice.other"
  | _ -> ());
  others
  |> List.iter (fun { loc; payload } ->
         match payload with
         | Unit -> ()
         | _ -> fail loc "@spice.other needs a constructor without payload");
  untagged
  |> List.iter (fun { loc; payload; aliases } ->
         (match payload with
         | Args [ _ ] -> ()
         | _ -> fail loc "@spice.untagged needs a constructor with one value");
         if aliases <> [] then
           fail loc "An @spice.untagged constructor has no name to alias");
  match (others, untagged) with
  | _ :: _, { loc } :: _ ->
      fail loc "@spice.other and @spice.untagged can't be combined"
  | _ -> ()

let validate ~tag cases =
  validate_names cases;
  validate_flags cases;
  match tag with
  | None -> ()
  | Some tag ->
      cases
      |> List.iter (fun { loc; payload } ->
             match payload with
             | Unit -> ()
             | Args [ _ ] -> ()
             | Args _ ->
                 fail loc
                   "An internally tagged (@tag) @spice.serde constructor can't \
                    have several payload values, like serde's #[serde(tag = \
                    ...)]; use an inline record, or one value that encodes to \
                    an object"
             | Record fields ->
                 let json_key { pld_name = { txt }; pld_attributes } =
                   match
                     get_attribute_by_name pld_attributes
                       (annotation_name ^ ".key")
                   with
                   | Ok (Some attribute) -> (
                       match (get_expression_from_payload attribute).pexp_desc with
                       | Pexp_constant (Pconst_string (key, _, _)) -> key
                       | _ -> txt)
                   | _ -> txt
                 in
                 if List.exists (fun field -> json_key field = tag) fields then
                   fail loc ("A payload field is keyed like the tag " ^ tag))

let encode_arg generator_settings arg name =
  let encoder, _ = Codecs.generate_value_codecs generator_settings arg in
  Exp.apply (Option.get encoder) [ (Nolabel, make_ident_expr name) ]

let encoder_case generator_settings ~tag { name; payload; lhs; untagged } =
  let rhs =
    match (payload, tag) with
    | Args [ arg ], _ when untagged -> encode_arg generator_settings arg "v0"
    | Unit, None -> [%expr JSON.String [%e str name]]
    | Unit, Some tag ->
        json_object [ [%expr [%e str tag], JSON.String [%e str name]] ]
    | Args args, Some tag ->
        [%expr
          Spice.taggedObject [%e str tag] [%e str name]
            [%e encode_arg generator_settings (List.hd args) "v0"]]
    | Args args, None ->
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
(* [(name, decoded)] for every JSON name a case reads, its aliases too. *)
let by_names f cases =
  cases
  |> List.concat_map (fun case ->
         match f case with
         | None -> []
         | Some decoded ->
             List.map
               (fun name -> (name, decoded name))
               (case.name :: case.aliases))

(* Decodes [v] as the first [@spice.untagged] constructor whose value it is,
   else gives [error]. *)
let rec decode_untagged generator_settings cases ~error =
  match cases with
  | [] -> error
  | { payload = Args [ arg ]; construct } :: rest ->
      let _, decoder = Codecs.generate_value_codecs generator_settings arg in
      [%expr
        match [%e Option.get decoder] v with
        | Ok v0 -> Ok [%e construct (Some [%expr v0])]
        | Error _ ->
            [%e decode_untagged generator_settings rest ~error]]
  | { loc } :: _ -> fail loc "@spice.untagged needs a constructor with one value"

let generate_decoder generator_settings ~tag ~legacy cases =
  (* serde writes a unit constructor as a bare string only when externally
     tagged; an internally tagged one reads it only as a legacy value. *)
  let string_decodes = Option.is_none tag || Option.is_some legacy in
  let untagged = List.filter (fun { untagged } -> untagged) cases in
  let cases = List.filter (fun { untagged } -> not untagged) cases in
  let other = List.find_opt (fun { other } -> other) cases in
  let invalid =
    match other with
    | Some { construct } -> [%expr Ok [%e construct None]]
    | None -> [%expr Spice.error "Invalid variant constructor" v]
  in
  let units =
    cases
    |> by_names (fun { payload; construct } ->
           match payload with
           | Unit -> Some (fun _ -> [%expr Ok [%e construct None]])
           | _ -> None)
  in
  (* As serde: a bare string naming a constructor with a payload is an
     error, not the [@spice.other] constructor. *)
  let names =
    match other with
    | None -> units
    | Some _ ->
        units
        @ (cases
          |> by_names (fun { payload } ->
                 match payload with
                 | Unit -> None
                 | _ ->
                     Some
                       (fun _ -> [%expr Spice.error "Invalid variant constructor" v])))
  in
  (* As serde: an unknown name keyed to anything but null is an error. *)
  let unknown_key =
    match other with
    | Some { construct } ->
        [%expr
          match payload with
          | JSON.Null -> Ok [%e construct None]
          | _ -> Spice.error ~path:("." ^ spice_tag) "Expected null" payload]
    | None -> invalid
  in
  let object_decoder =
    match tag with
    | None ->
        let with_payload =
          cases
          |> by_names (fun ({ constructor; payload } as case) ->
                 let decoded =
                   match payload with
                   | Unit ->
                       (* serde also reads a unit variant written as a map. *)
                       [%expr
                         match payload with
                         | JSON.Null -> Ok [%e case.construct None]
                         | _ -> Spice.error "Expected null" payload]
                   | Args args -> decode_args generator_settings case args
                   | Record fields ->
                       [%expr
                         match payload with
                         | JSON.Object dict ->
                             [%e
                               Records.generate_inline_record_decoder_expr
                                 generator_settings fields constructor]
                         | _ -> Spice.error "Not an object" payload]
                 in
                 Some (fun name -> with_path ("." ^ name) decoded))
        in
        [%expr
          match Dict.toArray dict with
          | [| (spice_tag, payload) |] ->
              [%e decode_by_name with_payload ~otherwise:unknown_key]
          | _ -> Spice.error "Expected an object with one key" v]
    | Some tag ->
        let records =
          cases
          |> by_names (fun ({ constructor; payload } as case) ->
                 match payload with
                 | Record fields ->
                     Some
                       (fun _ ->
                         Records.generate_inline_record_decoder_expr
                           generator_settings fields constructor)
                 | Args args ->
                     (* The payload decodes from the tagged object itself. *)
                     Some
                       (fun _ ->
                         [%expr
                           let payload = Spice.untagged [%e str tag] dict in
                           [%e decode_args generator_settings case args]])
                 | Unit -> None)
        in
        [%expr
          match Dict.get dict [%e str tag] with
          | Some (JSON.String spice_tag) ->
              [%e decode_by_name (units @ records) ~otherwise:invalid]
          | _ -> Spice.error ([%e str tag] ^ " missing") v]
  in
  let cases =
    [ [%expr
        (* First: JSON.Object alone would also match null here. *)
        function JSON.Null -> Spice.error "Not a variant" v] ]
    @ (if string_decodes then
         [ [%expr
             function
             | JSON.String spice_tag ->
                 [%e decode_by_name names ~otherwise:invalid]] ]
       else [])
    (* Before JSON.Object, which also matches arrays at runtime. *)
    @ (match legacy with
      | Some legacy ->
          [ [%expr
              function
              | JSON.Array [||] ->
                  Spice.error "Expected variant, found empty array" v];
            [%expr function JSON.Array json_arr -> [%e legacy]] ]
      | None -> [
            [%expr
              function
              | JSON.Array _ -> Spice.error "Expected variant, found array" v];
          ])
    @ [ [%expr function JSON.Object dict -> [%e object_decoder]] ]
    @ [ [%expr function _ -> Spice.error "Not a variant" v] ]
  in
  let cases =
    List.concat_map
      (function
        | { pexp_desc = Pexp_function cases } -> cases
        | { pexp_loc } -> fail pexp_loc "expected a function")
      cases
  in
  let decoded = Exp.match_ [%expr (v : JSON.t)] cases in
  let decoded =
    match untagged with
    | [] -> decoded
    | _ ->
        [%expr
          match [%e decoded] with
          | Ok _ as ok -> ok
          | Error _ as error ->
              [%e
                decode_untagged generator_settings untagged
                  ~error:[%expr error]]]
  in
  expr_func ~arity:1 [%expr fun v -> [%e decoded]]

let generate_codecs ({ do_encode; do_decode } as generator_settings) ~tag
    ~legacy cases =
  validate ~tag cases;
  ( (if do_encode then Some (generate_encoder generator_settings ~tag cases)
     else None),
    if do_decode then
      Some
        (generate_decoder generator_settings ~tag
           ~legacy:
             (match generator_settings.serde with
             | Some { legacy = true } -> Some (legacy ())
             | _ -> None)
           cases)
    else None )
