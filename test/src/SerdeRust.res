// Mirrors test/rust-serde/src/main.rs; fixtures/serde-rust.json is what
// Rust's serde derive writes for these types.

@spice.serde({legacy: true})
type size = [#S | #M | @spice.as("large") #L]

@spice.serde({legacy: true})
type poly = [#Left | #Right | #Payload(int) | #Pair(string, bool)]

@spice
type point = {x: int, y: int}

@spice.serde({legacy: true})
type rec ext =
  | Unit
  | @spice.as("renamed") Renamed
  | @spice.as("둘") Two
  | Single(int)
  | Float(float)
  | Pair(string, bool)
  | Rec({a: string, b: option<int>})
  | Opt(option<int>)
  | List(array<int>)
  | Map(dict<int>)
  | Nested(ext)
  | Point(point)

@spice.serde({legacy: true}) @tag("type")
type int_ =
  | Regular
  | Overlap({cameraSize: size})
  | @spice.as("full") Full({screenFit: string, @spice.key("punch_in") punchIn: bool})
  | Point(point)
  | Map(dict<int>)

@spice.serde({legacy: true}) @tag("kind")
type nested = SideBySide({position: poly, style: int_})

@spice.serde({legacy: true}) @tag("type")
type gen<'a> = G({value: 'a}) | E

@spice.serde({legacy: true})
type rec tree = Leaf | Node({left: tree, right: tree, value: int})

@spice.serde({legacy: true}) @tag("type")
type withNull = N({b: option<int>})

@spice.serde
type codec =
  | @as("prores") ProRes
  | @spice.untagged Other(string)
