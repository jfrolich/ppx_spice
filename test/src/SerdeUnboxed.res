// @unboxed: each value is in JSON what it is in memory.

@spice.serde @unboxed
type status =
  | @as("CREATED") Created
  | Done
  | Other(string)

@spice.serde @unboxed
type rec value =
  | @as("none") Empty
  | Flag(bool)
  | Number(float)
  | Text(string)
  | List(array<value>)
  | Rect({w: float, h: float})

@spice.serde @unboxed
type single = Single(int)
