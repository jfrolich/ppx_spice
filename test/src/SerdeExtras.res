// @spice.other: an unknown name decodes to it.
@spice.serde
type shadow =
  | @as("NONE") @spice.other NoShadow
  | @as("REGULAR") Regular
  | @as("DEEP") Deep

// @spice.alias: other names a constructor reads.
@spice.serde({legacy: true})
type scope =
  | @as("PUBLIC") @spice.alias("Public") Public
  | @as("ORG") @spice.alias("OrgListed") @spice.alias("ORG_LISTED") Org
  | @as("PRIVATE") @spice.alias("Invite") Private

// @spice.untagged: the value itself, for what no name reads.
@spice.serde
type codec =
  | @as("prores") ProRes
  | @spice.untagged Other(string)

@spice.serde @tag("type")
type shape =
  | Circle({radius: float})
  | @spice.other UnknownShape

@spice
type color = {r: int, g: int, b: int}

// @spice.fallback: anything the decoder rejects is this value.
@spice.serde @spice.fallback(Solid({r: 255, g: 255, b: 255}))
type background =
  | Solid(color)
  | Image(string)

@spice.serde
type poly = [@spice.other #Unknown | @spice.alias("left") #Left]

@spice.serde
type polyText = [#Left | @spice.untagged #Text(string)]

@spice @spice.fallback({r: 0, g: 0, b: 0})
type recordWithFallback = color

// @spice.other with constructors that have a payload, as serde reads them.
@spice.serde
type mixed =
  | Circle(float)
  | Rect({w: float})
  | @spice.other UnknownMixed

@spice.serde({legacy: true})
type legacyOther =
  | Known
  | @spice.other UnknownLegacy
