@spice.serde @tag("type")
type t = A | B({x: int})

@spice.serde
type p = [#A | #B(int)]
