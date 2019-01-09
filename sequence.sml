structure Sequence :> SEQUENCE =
struct
datatype 'a seq = consq of 'a * (unit -> 'a seq) | nilq

fun hdq (consq(x, xf)) = x
  | hdq nilq = raise Empty
fun tlq (consq(x, xf)) = xf()
  | tlq nilq = raise Empty

(* fold.q having "f : 'a -> (unit -> 'b) -> 'b"
   rather than just "f : 'a -> 'b -> b" makes
   more sense in foldrq than foldlq *)
fun foldlq f init nilq = init
  | foldlq f init (consq(x, xf)) =
    foldlq f (f (x, fn () => init)) (xf())
fun foldrq f init nilq = init
  | foldrq f init (consq(x, xf)) =
    f (x, fn () => foldrq f init (xf()))

fun appq (nilq, yf) = yf()
  | appq (consq(x, xf), yf) = consq(x, fn () => appq (xf(), yf))
fun interleaveq (nilq, yf) = yf()
  | interleaveq (consq(x, xf), yf) = consq(x, fn () => interleaveq (yf(), xf))

fun mapq f nilq = nilq
  | mapq f (consq(x, xf)) = consq(f x, fn () => mapq f (xf()))
fun mappq f seq = foldrq appq nilq (mapq f seq)

fun filterq f nilq = nilq
  | filterq f (consq(x, xf)) =
    if f x then consq(x, fn () => filterq f (xf())) else filterq f (xf())

fun listq seq = foldrq (fn (x,xf) => x::xf()) [] seq
fun seq list = foldr (fn (x,acc) => consq(x, fn () => acc))
                     nilq
                     list

fun takeq n nilq = nilq
  | takeq 0 _ = nilq
  | takeq n (consq(x, xf)) = consq(x, fn()=> takeq (n-1) (xf()))
fun dropq n nilq = nilq
  | dropq 0 seq = seq
  | dropq n (consq(x, xf)) = dropq (n-1) (xf())
end
