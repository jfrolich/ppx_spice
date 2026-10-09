// SerdeRust.ext and tree as plain @spice types: the same runtime values,
// encoded the way they were before switching to @spice.serde.

@spice
type point = {x: int, y: int}

@spice
type rec ext =
  | Unit
  | Renamed
  | Two
  | Single(int)
  | Float(float)
  | Pair(string, bool)
  | Rec({a: string, b: option<int>})
  | Opt(option<int>)
  | List(array<int>)
  | Map(dict<int>)
  | Nested(ext)
  | Point(point)

@spice
type rec tree = Leaf | Node({left: tree, right: tree, value: int})
