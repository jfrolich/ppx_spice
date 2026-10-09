// Edge cases for @spice.serde beyond the shapes Rust can produce.

@spice.serde
type rec expr = Num(int) | Add({left: expr, right: term}) | Neg(term)
@spice.serde @tag("op")
and term = Lit({value: int}) | Wrap({inner: expr}) | Zero

// Field names that are also names in the generated code.
@spice.serde
type ext = Clash({v: int, dict: int, payload: int, json_arr: int, spice_tag: int, e: int})

@spice.serde @tag("type")
type int_ = Clash({v: int, dict: int, payload: int, json_arr: int, spice_tag: int, e: int})

@spice
type record = {v: int, dict: int, e: int, decoder_a: int}

let upper: Spice.codec<string> = (
  s => JSON.String(s->String.toUpperCase),
  json =>
    switch json {
    | JSON.String(s) => Ok(s->String.toLowerCase)
    | _ => Spice.error("Not a string", json)
    },
)

@spice.serde @tag("type")
type fields =
  | Fields({
      name: @spice.codec(upper) string,
      @spice.default(7) count: int,
      optional?: string,
      nullable: Null.t<int>,
      list: list<int>,
      tuple: (int, string),
      result: result<int, string>,
    })

@spice.serde
type gen<'a, 'b> = One('a) | Two('a, 'b) | Rec({a: 'a, b: array<'b>}) | Nothing

@spice.serde @spice.decode
type decodeOnly = D1 | D2(int)

@spice.serde @spice.encode
type encodeOnly = E1 | E2(int)

@spice.serde
type single = Only

@spice.serde @tag("type")
type singleTagged = OnlyTagged({x: int})

@spice.serde
type unicode = | @spice.as("하나") Hana | @spice.as("with space") Space | @spice.as("") Empty

@spice.serde
type opt = Some_(option<option<int>>) | Arr(array<option<int>>)
