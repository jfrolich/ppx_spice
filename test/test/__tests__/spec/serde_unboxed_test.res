open Zora

let testEqual = (t, name, lhs, rhs) =>
  t->test(name, async t => {
    t->equal(lhs, rhs, name)
  })

let json = JSON.parseOrThrow

zoraBlock("@spice.serde @unboxed", t => {
  open SerdeUnboxed

  t->testEqual("status: named", status_decode(json(`"CREATED"`)), Ok(Created))
  t->testEqual("status: constructor name", status_decode(json(`"Done"`)), Ok(Done))
  t->testEqual("status: any other string", status_decode(json(`"WAITING"`)), Ok(Other("WAITING")))
  t->testEqual("status: encodes the name", status_encode(Created), json(`"CREATED"`))
  t->testEqual("status: encodes the value", status_encode(Other("WAITING")), json(`"WAITING"`))
  t->testEqual(
    "status: not a string fails",
    status_decode(json(`1`)),
    Error({path: "", message: "Not a variant", value: json(`1`)}),
  )

  let values = [
    Empty,
    Flag(true),
    Number(1.5),
    Text("none!"),
    List([Number(1.), Text("a"), List([]), Empty]),
    Rect({w: 1., h: 2.}),
  ]
  values->Array.forEach(v => {
    let encoded = value_encode(v)
    t->testEqual(`value: ${JSON.stringify(encoded)} is the value in memory`, encoded, Obj.magic(v))
    t->testEqual(`value: ${JSON.stringify(encoded)} decodes back`, value_decode(encoded), Ok(v))
  })
  t->testEqual("value: a name decodes to it", value_decode(json(`"none"`)), Ok(Empty))
  t->testEqual("value: a nested error fails", value_decode(json(`[null]`))->Result.isError, true)
  t->testEqual(
    "value: a record missing a field fails",
    value_decode(json(`{"w": 1}`))->Result.isError,
    true,
  )
  t->testEqual("value: null fails", value_decode(json(`null`))->Result.isError, true)

  t->testEqual("single: the value alone", single_encode(Single(3)), json(`3`))
  t->testEqual("single: decodes", single_decode(json(`3`)), Ok(Single(3)))
})
