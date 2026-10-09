// Seeded random values: each round-trips through @spice.serde, and the same
// value as written by plain @spice (the legacy encoding) decodes to it too.
open Zora

let seed = ref(42)
let next = bound => {
  seed := Int.bitwiseAnd(seed.contents * 1103515245 + 12345, 0x7fffffff)
  mod(seed.contents / 65536, bound)
}
let int = () => next(2001) - 1000
let string = () => ["", "a", "둘", `q"uote`, "back\\slash", "type"][next(6)]->Option.getOr("")

let rec ext = (depth): SerdeRust.ext =>
  switch next(depth > 3 ? 11 : 12) {
  | 0 => Unit
  | 1 => Renamed
  | 2 => Two
  | 3 => Single(int())
  | 4 => Float(Float.fromInt(int()) /. 4.)
  | 5 => Pair(string(), next(2) == 0)
  | 6 => Rec({a: string(), b: next(2) == 0 ? None : Some(int())})
  | 7 => Opt(next(2) == 0 ? None : Some(int()))
  | 8 => List(Array.fromInitializer(~length=next(4), _ => int()))
  | 9 => Map(Array.fromInitializer(~length=next(3), _ => (string(), int()))->Dict.fromArray)
  | 10 => Point({x: int(), y: int()})
  | _ => Nested(ext(depth + 1))
  }

let rec tree = (depth): SerdeRust.tree =>
  depth > 4 || next(3) == 0
    ? Leaf
    : Node({left: tree(depth + 1), right: tree(depth + 1), value: int()})

// Absent optional fields come back as keys holding undefined; compare
// re-encoded JSON (a canonical form) as well as by ReScript equality.
let check = (t, name, values, encode, decode, legacyEncode) =>
  t->test(name, async t => {
    let failures = values->Array.filter(v => {
      let json = encode(v)
      decode(json)->Result.map(encode) != Ok(json) ||
        decode(legacyEncode(v))->Result.map(encode) != Ok(json)
    })

    t->equal(failures, [], name)
  })

zoraBlock("@spice.serde random round trips", t => {
  let exts = Array.fromInitializer(~length=500, _ => ext(0))
  let trees = Array.fromInitializer(~length=200, _ => tree(0))

  t->check(
    "ext",
    exts,
    SerdeRust.ext_encode,
    SerdeRust.ext_decode,
    v => SerdeLegacy.ext_encode(Obj.magic(v)),
  )
  t->check(
    "tree",
    trees,
    SerdeRust.tree_encode,
    SerdeRust.tree_decode,
    v => SerdeLegacy.tree_encode(Obj.magic(v)),
  )
  t->test("generated every ext constructor", async t => {
    let tags = exts->Array.map(v => (Obj.magic(v): {"TAG": Nullable.t<string>})["TAG"])
    t->equal(
      Set.fromArray(tags->Array.map(tag => tag->Nullable.toOption->Option.getOr("unit")))->Set.size,
      10,
      "distinct payload constructors plus units",
    )
  })
})
