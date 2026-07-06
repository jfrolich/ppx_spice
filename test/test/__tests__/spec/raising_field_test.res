open Zora

let testEqual = (t, name, lhs, rhs) =>
  t->test(name, async t => {
    t->equal(lhs, rhs, name)
  })

zoraBlock("option field with a raising legacy codec", t => {
  let sample = dict{
    "label": JSON.Null,
    "value": JSON.Null,
  }
  let sampleJson = sample->JSON.Object
  let decoded = sampleJson->RaisingField.t_decode
  t->test("null decodes to None when the inner decoder raises", async t => {
    switch decoded {
    | Ok(record) => {
        t->equal(record.label, None, "label")
        t->equal(record.value, None, "value")
      }
    | Error(_) => t->fail("expected decode to succeed")
    }
  })

  let sample2 = dict{
    "label": JSON.String("a"),
    "value": JSON.String("b"),
  }
  let sampleJson2 = sample2->JSON.Object
  let decoded2 = sampleJson2->RaisingField.t_decode
  t->test("present values still decode", async t => {
    switch decoded2 {
    | Ok(record) => {
        t->equal(record.label, Some("a"), "label")
        t->equal(record.value, Some("b"), "value")
      }
    | Error(_) => t->fail("expected decode to succeed")
    }
  })

  let encoded = ({label: Some("a"), value: "b"}: RaisingField.t)->RaisingField.t_encode
  t->testEqual(
    "encode",
    encoded,
    dict{"label": JSON.String("a"), "value": JSON.String("b")}->JSON.Object,
  )
})
