// Every value encodes to exactly what Rust's serde derive writes for it
// (fixtures/serde-rust.json, from test/rust-serde) and decodes back from it.
open Zora

@module("node:fs") external readFileSync: (string, string) => string = "readFileSync"

let golden =
  readFileSync("fixtures/serde-rust.json", "utf8")
  ->JSON.parseOrThrow
  ->JSON.Decode.object
  ->Option.getOrThrow

let used = ref([])

let check = (t, name, value, encode, decode) => {
  used := used.contents->Array.concat([name])
  let json = golden->Dict.get(name)->Option.getOrThrow

  t->test(name, async t => {
    t->equal(encode(value), json, name ++ " encodes like serde")
    t->equal(decode(json), Ok(value), name ++ " decodes serde's JSON")
  })
}

zoraBlock("@spice.serde matches Rust serde", t => {
  open SerdeRust
  let size = (name, v) => check(t, name, v, size_encode, size_decode)
  let poly = (name, v) => check(t, name, v, poly_encode, poly_decode)
  let ext = (name, v) => check(t, name, v, ext_encode, ext_decode)
  let int = (name, v) => check(t, name, v, int__encode, int__decode)
  let gen = (name, v) =>
    check(
      t,
      name,
      v,
      gen_encode(Spice.arrayToJson(Spice.stringToJson, ...)),
      gen_decode(Spice.arrayFromJson(Spice.stringFromJson, ...)),
    )

  size("size_s", #S)
  size("size_l", #L)
  poly("poly_left", #Left)
  poly("poly_right", #Right)
  poly("poly_payload", #Payload(-3))
  poly("poly_pair", #Pair(`a"b`, true))
  ext("ext_unit", Unit)
  ext("ext_renamed", Renamed)
  ext("ext_two", Two)
  ext("ext_single", Single(7))
  ext("ext_float", Float(1.5))
  ext("ext_pair", Pair("x", false))
  ext("ext_rec", Rec({a: "a", b: Some(2)}))
  ext("ext_rec_none", Rec({a: "a", b: None}))
  ext("ext_opt_some", Opt(Some(1)))
  ext("ext_opt_none", Opt(None))
  ext("ext_list", List([1, 2, 3]))
  ext("ext_list_empty", List([]))
  ext("ext_map", Map(Dict.fromArray([("a", 1), ("b", 2)])))
  ext("ext_nested", Nested(Nested(Unit)))
  ext("ext_point", Point({x: 1, y: -2}))
  int("int_regular", Regular)
  int("int_overlap", Overlap({cameraSize: #L}))
  int("int_full", Full({screenFit: "cover", punchIn: true}))
  int("int_point", Point({x: 3, y: 4}))
  int("int_map", Map(Dict.fromArray([("k", 5)])))
  int("int_map_empty", Map(Dict.make()))
  check(
    t,
    "nested",
    SideBySide({position: #Pair("p", false), style: Overlap({cameraSize: #M})}),
    nested_encode,
    nested_decode,
  )
  gen("gen_g", G({value: ["a", "b"]}))
  gen("gen_e", E)
  check(
    t,
    "tree",
    Node({left: Leaf, right: Node({left: Leaf, right: Leaf, value: 2}), value: 1}),
    tree_encode,
    tree_decode,
  )

  t->test("with_null: serde's null for None decodes", async t => {
    t->equal(
      golden->Dict.get("with_null")->Option.getOrThrow->withNull_decode,
      Ok(N({b: None})),
      "with_null",
    )
  })
  used := used.contents->Array.concat(["with_null"])

  t->test("every golden case is checked", async t => {
    t->equal(
      used.contents->Array.toSorted(String.compare),
      golden->Dict.keysToArray->Array.toSorted(String.compare),
      "cases",
    )
  })
})
