open Zora

let testEqual = (t, name, lhs, rhs) =>
  t->test(name, async t => {
    t->equal(lhs, rhs, name)
  })

let json = JSON.parseOrThrow

zoraBlock("@spice.other, @spice.alias, @spice.untagged, @spice.fallback", t => {
  open SerdeExtras

  t->testEqual("other: known", shadow_decode(json(`"DEEP"`)), Ok(Deep))
  t->testEqual("other: unknown name", shadow_decode(json(`"GLOW"`)), Ok(NoShadow))
  t->testEqual("other: unknown object", shadow_decode(json(`{"GLOW": null}`)), Ok(NoShadow))
  t->testEqual("other: encodes its name", shadow_encode(NoShadow), json(`"NONE"`))
  t->testEqual(
    "other: not a name still fails",
    shadow_decode(json(`1`))->Result.isError,
    true,
  )
  t->testEqual("other: tagged", shape_decode(json(`{"type": "Square"}`)), Ok(UnknownShape))
  t->testEqual(
    "other: tagged known",
    shape_decode(json(`{"type": "Circle", "radius": 1}`)),
    Ok(Circle({radius: 1.})),
  )

  t->testEqual("alias: name", scope_decode(json(`"ORG"`)), Ok(Org))
  t->testEqual("alias: alias", scope_decode(json(`"OrgListed"`)), Ok(Org))
  t->testEqual("alias: second alias", scope_decode(json(`"ORG_LISTED"`)), Ok(Org))
  t->testEqual("alias: legacy array", scope_decode(json(`["Invite"]`)), Ok(Private))
  t->testEqual("alias: legacy constructor", scope_decode(json(`["Public"]`)), Ok(Public))
  t->testEqual("alias: encodes the name", scope_encode(Org), json(`"ORG"`))
  t->testEqual(
    "alias: unknown fails",
    scope_decode(json(`"Nope"`))->Result.isError,
    true,
  )

  t->testEqual("untagged: named", codec_decode(json(`"prores"`)), Ok(ProRes))
  t->testEqual("untagged: value", codec_decode(json(`"h264"`)), Ok(Other("h264")))
  t->testEqual("untagged: encodes the value", codec_encode(Other("h264")), json(`"h264"`))
  t->testEqual("untagged: named encodes", codec_encode(ProRes), json(`"prores"`))
  t->testEqual(
    "untagged: keeps the tagged error",
    codec_decode(json(`1`)),
    Error({path: "", message: "Not a variant", value: json(`1`)}),
  )
  t->testEqual(
    "untagged: its name is no tag",
    codec_decode(json(`{"Other": "x"}`))->Result.isError,
    true,
  )

  t->testEqual("fallback: decodes", background_decode(json(`{"Image": "u"}`)), Ok(Image("u")))
  t->testEqual(
    "fallback: unknown",
    background_decode(json(`{"Ambient": null}`)),
    Ok(Solid({r: 255, g: 255, b: 255})),
  )
  t->testEqual(
    "fallback: record",
    recordWithFallback_decode(json(`{"r": 1}`)),
    Ok({r: 0, g: 0, b: 0}),
  )

  t->testEqual("poly other", poly_decode(json(`"Right"`)), Ok(#Unknown))
  t->testEqual("poly alias", poly_decode(json(`"left"`)), Ok(#Left))
  t->testEqual("poly untagged", polyText_decode(json(`"Right"`)), Ok(#Text("Right")))
  t->testEqual("poly untagged named", polyText_decode(json(`"Left"`)), Ok(#Left))
  t->testEqual("poly untagged fails", polyText_decode(json(`5`))->Result.isError, true)
  t->testEqual("poly untagged encode", polyText_encode(#Text("a")), json(`"a"`))

  t->testEqual("other: unknown name keyed to null", mixed_decode(json(`{"Square": null}`)), Ok(UnknownMixed))
  t->testEqual(
    "other: unknown name keyed to a value fails, like serde",
    mixed_decode(json(`{"Square": 5}`)),
    Error({path: ".Square", message: "Expected null", value: json(`5`)}),
  )
  t->testEqual(
    "other: a payload constructor's bare name fails, like serde",
    mixed_decode(json(`"Rect"`)),
    Error({path: "", message: "Invalid variant constructor", value: json(`"Rect"`)}),
  )
  t->testEqual("other: unknown bare name", mixed_decode(json(`"Square"`)), Ok(UnknownMixed))
  t->testEqual("other: legacy unknown", legacyOther_decode(json(`["Zzz"]`)), Ok(UnknownLegacy))
  t->testEqual(
    "other: legacy unknown with arguments fails",
    legacyOther_decode(json(`["Zzz", 1]`)),
    Error({
      path: "",
      message: "Invalid number of arguments to variant constructor",
      value: json(`["Zzz", 1]`),
    }),
  )
})
