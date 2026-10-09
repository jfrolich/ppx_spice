open Zora

let testEqual = (t, name, lhs, rhs) =>
  t->test(name, async t => {
    t->equal(lhs, rhs, name)
  })

let json = JSON.parseOrThrow
let errorAt = result =>
  switch result {
  | Ok(_) => None
  | Error({Spice.path: path, message}) => Some((path, message))
  }

let roundTrips = (t, name, values, encode, decode) =>
  t->testEqual(
    name,
    values->Array.map(v => v->encode->decode),
    values->Array.map(v => Ok(v)),
  )

zoraBlock("@spice.serde mutually recursive types", t => {
  open SerdeEdge
  let value = Add({left: Num(1), right: Wrap({inner: Neg(Zero)})})

  t->testEqual(
    "encode mixes external and internal tags",
    value->expr_encode,
    json(`{"Add":{"left":{"Num":1},"right":{"op":"Wrap","inner":{"Neg":{"op":"Zero"}}}}}`),
  )
  t->roundTrips(
    "round trip",
    [value, Num(-1), Neg(Lit({value: 2})), Add({left: value, right: Zero})],
    expr_encode,
    expr_decode,
  )
  t->testEqual(
    "external error path",
    json(`{"Add":{"left":{"Num":"x"},"right":"Zero"}}`)->expr_decode->errorAt,
    Some((".Add.left.Num", "Not a number")),
  )
  t->testEqual(
    "internal error path",
    json(`{"Add":{"left":{"Num":1},"right":{"op":"Lit","value":"no"}}}`)->expr_decode->errorAt,
    Some((".Add.right.value", "Not a number")),
  )
})

zoraBlock("@spice.serde field names used by generated code", t => {
  open SerdeEdge
  let fields = `{"v":1,"dict":2,"payload":3,"json_arr":4,"spice_tag":5,"e":6}`

  t->testEqual(
    "externally tagged",
    json(`{"Clash":${fields}}`)->ext_decode,
    Ok(Clash({v: 1, dict: 2, payload: 3, json_arr: 4, spice_tag: 5, e: 6})),
  )
  t->testEqual(
    "internally tagged",
    json(`{"type":"Clash",${fields->String.slice(~start=1)}`)->int__decode,
    Ok(Clash({v: 1, dict: 2, payload: 3, json_arr: 4, spice_tag: 5, e: 6})),
  )
  t->testEqual(
    "plain @spice record",
    json(`{"v":1,"dict":2,"e":3,"decoder_a":4}`)->record_decode,
    Ok({v: 1, dict: 2, e: 3, decoder_a: 4}),
  )
})

zoraBlock("@spice.serde record field attributes and types", t => {
  open SerdeEdge
  let value = Fields({
    name: "ab",
    count: 1,
    optional: "o",
    nullable: Null.Value(3),
    list: list{1, 2},
    tuple: (1, "x"),
    result: Error("e"),
  })

  t->testEqual(
    "encode",
    value->fields_encode,
    json(`{"type":"Fields","name":"AB","count":1,"optional":"o","nullable":3,"list":[1,2],"tuple":[1,"x"],"result":["Error","e"]}`),
  )
  t->roundTrips(
    "round trip",
    [value, Fields({name: "", count: 0, optional: "", nullable: Null.null, list: list{}, tuple: (0, ""), result: Ok(0)})],
    fields_encode,
    fields_decode,
  )
  // Compared re-encoded: a decoded absent optional field is a key holding
  // undefined, which deep equality tells apart from no key (plain spice too).
  t->testEqual(
    "@spice.default and an absent optional field",
    json(`{"type":"Fields","name":"AB","nullable":null,"list":[],"tuple":[1,"x"],"result":["Ok",1]}`)
    ->fields_decode
    ->Result.map(fields_encode),
    Ok(json(`{"type":"Fields","name":"AB","count":7,"nullable":null,"list":[],"tuple":[1,"x"],"result":["Ok",1]}`)),
  )
  t->testEqual(
    "a missing required field",
    json(`{"type":"Fields","name":"AB","list":[],"tuple":[1,"x"],"result":["Ok",1]}`)->fields_decode->errorAt,
    Some((".nullable", "nullable missing")),
  )
})

zoraBlock("@spice.serde generic types", t => {
  open SerdeEdge
  let encode = gen_encode(Spice.intToJson)(Spice.stringToJson)
  let decode = gen_decode(Spice.intFromJson)(Spice.stringFromJson)

  t->testEqual(
    "encode",
    [One(1), Two(1, "b"), Rec({a: 1, b: ["x"]}), Nothing]->Array.map(encode),
    [json(`{"One":1}`), json(`{"Two":[1,"b"]}`), json(`{"Rec":{"a":1,"b":["x"]}}`), json(`"Nothing"`)],
  )
  t->roundTrips("round trip", [One(1), Two(1, "b"), Rec({a: 1, b: []}), Nothing], encode, decode)
  t->testEqual("legacy", json(`["Two",1,"b"]`)->decode, Ok(Two(1, "b")))
  t->testEqual(
    "tuple error path",
    json(`{"Two":[1,2]}`)->decode->errorAt,
    Some((".Two[1]", "Not a string")),
  )
  t->testEqual(
    "record error path",
    json(`{"Rec":{"a":1,"b":[2]}}`)->decode->errorAt,
    Some((".Rec.b[0]", "Not a string")),
  )
})

zoraBlock("@spice.serde options in payloads", t => {
  open SerdeEdge
  t->testEqual("None is null", Some_(None)->opt_encode, json(`{"Some_":null}`))
  t->testEqual("null is None", json(`{"Some_":null}`)->opt_decode, Ok(Some_(None)))
  t->testEqual("a value is Some", json(`{"Some_":3}`)->opt_decode, Ok(Some_(Some(Some(3)))))
  t->testEqual("options in an array", Arr([Some(1), None])->opt_encode, json(`{"Arr":[1,null]}`))
  t->testEqual("an array with nulls", json(`{"Arr":[1,null]}`)->opt_decode, Ok(Arr([Some(1), None])))
})

zoraBlock("@spice.serde names", t => {
  open SerdeEdge
  t->testEqual("non-ASCII", Hana->unicode_encode, json(`"하나"`))
  t->testEqual("with a space", Space->unicode_encode, json(`"with space"`))
  t->testEqual("empty", Empty->unicode_encode, json(`""`))
  t->roundTrips("round trip", [Hana, Space, Empty], unicode_encode, unicode_decode)
  t->testEqual("a single constructor", Only->single_encode, json(`"Only"`))
  t->testEqual(
    "a single tagged constructor",
    OnlyTagged({x: 1})->singleTagged_encode,
    json(`{"type":"OnlyTagged","x":1}`),
  )
})

zoraBlock("@spice.serde decode and encode only", t => {
  open SerdeEdge
  t->testEqual("decode only", json(`{"D2":1}`)->decodeOnly_decode, Ok(D2(1)))
  t->testEqual("encode only", E2(1)->encodeOnly_encode, json(`{"E2":1}`))
})

zoraBlock("@spice.serde with an interface", t => {
  t->testEqual("variant", SerdeIface.B({x: 1})->SerdeIface.t_encode, json(`{"type":"B","x":1}`))
  t->testEqual("polyvariant", #B(1)->SerdeIface.p_encode, json(`{"B":1}`))
  t->testEqual("decode", json(`{"type":"A"}`)->SerdeIface.t_decode, Ok(SerdeIface.A))
})

zoraBlock("@spice.serde rejected input", t => {
  open SerdeEdge
  let expr = s => json(s)->expr_decode->errorAt
  let term = s => json(s)->term_decode->errorAt

  t->testEqual("a number", expr(`5`), Some(("", "Not a variant")))
  t->testEqual("a boolean", expr(`true`), Some(("", "Not a variant")))
  t->testEqual("null", expr(`null`), Some(("", "Not a variant")))
  t->testEqual("tagged null", term(`null`), Some(("", "Not a variant")))
  t->testEqual("a polyvariant null", json(`null`)->Serde.poly_decode->errorAt, Some(("", "Not a variant")))
  t->testEqual("a null record payload", expr(`{"Add":null}`), Some((".Add", "Not an object")))
  t->testEqual("a null single payload", expr(`{"Num":null}`), Some((".Num", "Not a number")))
  t->testEqual(
    "a null tuple payload",
    gen_decode(Spice.intFromJson)(Spice.stringFromJson)(json(`{"Two":null}`))->errorAt,
    Some((".Two", "Not an array")),
  )
  t->testEqual("a null inside a tagged single value", json(`{"type":"Point","x":null}`)->Serde.internal_decode->errorAt, Some((".x", "Not a number")))
  t->testEqual("an empty array", expr(`[]`), Some(("", "Expected variant, found empty array")))
  t->testEqual("an empty object", expr(`{}`), Some(("", "Expected an object with one key")))
  t->testEqual("two keys", expr(`{"Num":1,"Neg":"Zero"}`), Some(("", "Expected an object with one key")))
  t->testEqual("an unknown name", expr(`{"Mul":1}`), Some(("", "Invalid variant constructor")))
  t->testEqual("a payload name as a string", expr(`"Num"`), Some(("", "Invalid variant constructor")))
  t->testEqual("a tuple that isn't an array", gen_decode(Spice.intFromJson)(Spice.stringFromJson)(json(`{"Two":1}`))->errorAt, Some((".Two", "Not an array")))
  t->testEqual("a record that isn't an object", expr(`{"Add":1}`), Some((".Add", "Not an object")))
  t->testEqual("a unit with a value", json(`{"Only":1}`)->single_decode->errorAt, Some((".Only", "Expected null")))
  t->testEqual("a unit with null", json(`{"Only":null}`)->single_decode, Ok(Only))
  t->testEqual("legacy wrong arity", expr(`["Num"]`), Some(("", "Invalid number of arguments to variant constructor")))
  t->testEqual("legacy unknown name", expr(`["Mul",1]`), Some(("", "Invalid variant constructor")))
  t->testEqual("a missing tag", term(`{"value":1}`), Some(("", "op missing")))
  t->testEqual("a tag that isn't a string", term(`{"op":1}`), Some(("", "op missing")))
  t->testEqual("an unknown tag", term(`{"op":"Mul"}`), Some(("", "Invalid variant constructor")))
  t->testEqual("an unknown bare name", term(`"Mul"`), Some(("", "Invalid variant constructor")))
  t->testEqual("extra keys on a unit are ignored, like serde", json(`{"op":"Zero","x":1}`)->term_decode, Ok(Zero))
})
