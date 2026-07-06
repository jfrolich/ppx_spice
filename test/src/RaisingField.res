module RaisingCodec = {
  exception DecodeError(string)

  type t = string
  let t_encode = (v: t): JSON.t => JSON.String(v)
  let t_encodeJson = t_encode
  let t_decode = (json: JSON.t): result<t, Spice.decodeError> =>
    switch json {
    | JSON.String(s) => Ok(s)
    | _ => throw(DecodeError("Expected string"))
    }
}

@spice
type t = {
  label: option<RaisingCodec.t>,
  value?: RaisingCodec.t,
}
