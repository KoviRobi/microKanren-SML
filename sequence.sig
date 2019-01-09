signature SEQUENCE =
sig
    datatype 'a seq = consq of 'a * (unit -> 'a seq) | nilq
    val seq : 'a list -> 'a seq
    val hdq : 'a seq -> 'a
    val tlq : 'a seq -> 'a seq
    val foldlq : ('a * (unit -> 'b) -> 'b) -> 'b -> 'a seq -> 'b
    val foldrq : ('a * (unit -> 'b) -> 'b) -> 'b -> 'a seq -> 'b
    val mapq : ('a -> 'b) -> 'a seq -> 'b seq
    (* Note how the second argument to appq is a function *)
    val appq : 'a seq * (unit -> 'a seq) -> 'a seq
    (* mappq stands for map and append sequence *)
    val mappq : ('a -> 'b seq) -> 'a seq -> 'b seq
    (* interleaveq([a0, a1, ...], [b0, b1, ...]) = [a0, b0, a1, b1, ...] *)
    val interleaveq : 'a seq * (unit -> 'a seq) -> 'a seq
    val filterq : ('a -> bool) -> 'a seq -> 'a seq
    val listq : 'a seq -> 'a list
    val takeq : int -> 'a seq -> 'a seq
    val dropq : int -> 'a seq -> 'a seq
end
