// Mirrors test/rust-serde/src/main.rs; fixtures/serde-rust.json is what
// Rust's serde derive writes for these types.

@spice.serde
type size = [#S | #M | @spice.as("large") #L]

@spice.serde
type poly = [#Left | #Right | #Payload(int) | #Pair(string, bool)]

@spice
type point = {x: int, y: int}

@spice.serde
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

@spice.serde @tag("type")
type int_ =
  | Regular
  | Overlap({cameraSize: size})
  | @spice.as("full") Full({screenFit: string, @spice.key("punch_in") punchIn: bool})
  | Point(point)
  | Map(dict<int>)

@spice.serde @tag("kind")
type nested = SideBySide({position: poly, style: int_})

@spice.serde @tag("type")
type gen<'a> = G({value: 'a}) | E

@spice.serde
type rec tree = Leaf | Node({left: tree, right: tree, value: int})

@spice.serde @tag("type")
type withNull = N({b: option<int>})
