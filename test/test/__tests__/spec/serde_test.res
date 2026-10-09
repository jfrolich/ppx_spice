open Zora

let testEqual = (t, name, lhs, rhs) =>
  t->test(name, async t => {
    t->equal(lhs, rhs, name)
  })

let json = JSON.parseOrThrow
let isError = result =>
  switch result {
  | Ok(_) => false
  | Error(_) => true
  }

zoraBlock("@spice.serde polymorphic variants", t => {
  t->testEqual("encode unit", #Left->Serde.poly_encode, json(`"Left"`))
  t->testEqual("encode renamed", #L->Serde.size_encode, json(`"large"`))
  t->testEqual("encode payload", #Payload(1)->Serde.poly_encode, json(`{"Payload":1}`))
  t->testEqual("encode tuple", #Pair("a", true)->Serde.poly_encode, json(`{"Pair":["a",true]}`))

  t->testEqual("decode unit", json(`"Right"`)->Serde.poly_decode, Ok(#Right))
  t->testEqual("decode renamed", json(`"large"`)->Serde.size_decode, Ok(#L))
  t->testEqual("decode payload", json(`{"Payload":2}`)->Serde.poly_decode, Ok(#Payload(2)))
  t->testEqual(
    "decode tuple",
    json(`{"Pair":["b",false]}`)->Serde.poly_decode,
    Ok(#Pair("b", false)),
  )
  t->testEqual("decode legacy unit", json(`["Left"]`)->Serde.poly_decode, Ok(#Left))
  t->testEqual("decode legacy payload", json(`["Payload",3]`)->Serde.poly_decode, Ok(#Payload(3)))
  t->testEqual("unknown name fails", json(`"Up"`)->Serde.poly_decode->isError, true)
})

zoraBlock("@spice.serde externally tagged variants", t => {
  t->testEqual("encode unit", Serde.Unit->Serde.external__encode, json(`"Unit"`))
  t->testEqual("encode renamed", Serde.Renamed->Serde.external__encode, json(`"renamed"`))
  t->testEqual("encode single", Serde.Single(1)->Serde.external__encode, json(`{"Single":1}`))
  t->testEqual(
    "encode tuple",
    Serde.Pair("a", true)->Serde.external__encode,
    json(`{"Pair":["a",true]}`),
  )
  t->testEqual(
    "encode record",
    Serde.Rec({a: "x", b: None})->Serde.external__encode,
    json(`{"Rec":{"a":"x"}}`),
  )

  let decode = s => json(s)->Serde.external__decode
  t->testEqual("decode unit", decode(`"Unit"`), Ok(Serde.Unit))
  t->testEqual("decode renamed", decode(`"renamed"`), Ok(Serde.Renamed))
  t->testEqual("decode single", decode(`{"Single":1}`), Ok(Serde.Single(1)))
  t->testEqual("decode tuple", decode(`{"Pair":["a",true]}`), Ok(Serde.Pair("a", true)))
  t->testEqual("decode record", decode(`{"Rec":{"a":"x","b":2}}`), Ok(Serde.Rec({a: "x", b: Some(2)})))

  t->testEqual("decode legacy unit", decode(`["Unit"]`), Ok(Serde.Unit))
  t->testEqual("decode legacy tuple", decode(`["Pair","a",true]`), Ok(Serde.Pair("a", true)))
  t->testEqual("decode legacy record", decode(`["Rec",{"a":"x"}]`), Ok(Serde.Rec({a: "x", b: None})))

  t->testEqual(
    "payload errors carry the constructor in the path",
    decode(`{"Single":"no"}`)->Result.mapError(e => e.path),
    Error(".Single"),
  )
  t->testEqual("two keys fail", decode(`{"Single":1,"Unit":2}`)->isError, true)
  t->testEqual("wrong tuple length fails", decode(`{"Pair":["a"]}`)->isError, true)
  t->testEqual("a unit name with an object fails", decode(`{"Unit":1}`)->isError, true)
})

zoraBlock("@spice.serde internally tagged variants (@tag)", t => {
  t->testEqual("encode unit", Serde.Regular->Serde.internal_encode, json(`{"type":"Regular"}`))
  t->testEqual(
    "encode record",
    Serde.Overlap({cameraSize: #M})->Serde.internal_encode,
    json(`{"type":"Overlap","cameraSize":"M"}`),
  )
  t->testEqual(
    "encode renamed with field keys",
    Serde.Full({screenFit: "cover", punchIn: true})->Serde.internal_encode,
    json(`{"type":"full","screenFit":"cover","punch_in":true}`),
  )
  t->testEqual(
    "encode nested",
    Serde.SideBySide({position: #Left, style: Serde.Regular})->Serde.nested_encode,
    json(`{"kind":"SideBySide","position":"Left","style":{"type":"Regular"}}`),
  )

  let decode = s => json(s)->Serde.internal_decode
  t->testEqual("decode unit", decode(`{"type":"Regular"}`), Ok(Serde.Regular))
  t->testEqual(
    "decode record in any key order",
    decode(`{"cameraSize":"large","type":"Overlap"}`),
    Ok(Serde.Overlap({cameraSize: #L})),
  )
  t->testEqual(
    "decode renamed",
    decode(`{"type":"full","screenFit":"cover","punch_in":false}`),
    Ok(Serde.Full({screenFit: "cover", punchIn: false})),
  )
  t->testEqual("decode a bare unit name", decode(`"Regular"`), Ok(Serde.Regular))
  t->testEqual(
    "decode legacy record",
    decode(`["Overlap",{"cameraSize":["S"]}]`),
    Ok(Serde.Overlap({cameraSize: #S})),
  )
  t->testEqual(
    "decode nested legacy",
    json(`["SideBySide",{"position":["Payload",1],"style":["Regular"]}]`)->Serde.nested_decode,
    Ok(Serde.SideBySide({position: #Payload(1), style: Serde.Regular})),
  )
  t->testEqual(
    "field errors keep their path",
    decode(`{"type":"Overlap","cameraSize":"XL"}`)->Result.mapError(e => e.path),
    Error(".cameraSize"),
  )
  t->testEqual(
    "encode a single value into its object",
    Serde.Point({x: 1, y: 2})->Serde.internal_encode,
    json(`{"type":"Point","x":1,"y":2}`),
  )
  t->testEqual(
    "decode a single value from the tagged object",
    decode(`{"y":2,"type":"Point","x":1}`),
    Ok(Serde.Point({x: 1, y: 2})),
  )
  t->testEqual(
    "decode a legacy single value",
    decode(`["Point",{"x":1,"y":2}]`),
    Ok(Serde.Point({x: 1, y: 2})),
  )
  t->testEqual(
    "a single value that isn't an object fails to encode",
    switch Serde.NotAnObject(1)->Serde.internal_encode {
    | exception _ => true
    | _ => false
    },
    true,
  )
  t->testEqual(
    "a single value that isn't an object fails to decode",
    decode(`{"type":"NotAnObject"}`)->isError,
    true,
  )
  t->testEqual(
    "a single value whose object has the tag key fails to encode",
    switch Serde.Clash({kind: "k"})->Serde.internal_encode {
    | exception _ => true
    | _ => false
    },
    true,
  )
  t->testEqual("missing tag fails", decode(`{"cameraSize":"S"}`)->isError, true)
  t->testEqual("unknown tag fails", decode(`{"type":"Other"}`)->isError, true)
})
