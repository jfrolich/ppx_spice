@spice.serde
type size = [#S | #M | @spice.as("large") #L]

@spice.serde
type poly = [#Left | #Right | #Payload(int) | #Pair(string, bool)]

@spice.serde
type external_ =
  | Unit
  | @spice.as("renamed") Renamed
  | Single(int)
  | Pair(string, bool)
  | Rec({a: string, b: option<int>})

@spice
type point = {x: int, y: int}

@spice
type withType = {@spice.key("type") kind: string}

@spice.serde @tag("type")
type internal =
  | Clash(withType)
  | Point(point)
  | NotAnObject(int)
  | Regular
  | Overlap({cameraSize: size})
  | @spice.as("full") Full({screenFit: string, @spice.key("punch_in") punchIn: bool})

@spice.serde @tag("kind")
type nested = SideBySide({position: poly, style: internal})
