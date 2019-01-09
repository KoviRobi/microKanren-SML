(* The following are shorthands *)
fun comp s l = Comp(s, l)
val [A,B,C, E,F,G, L,N, P,Q,R, X,Y,Z] =
    map (Var o User)
        ["A","B","C","E","F","G","L","N","P","Q","R","X","Y","Z"]
val [A1,A2,A3,A4,A5,A6,A7] = map (Atom o Int.toString) [1,2,3,4,5,6,7]
val [A_nil, Ax, Ay, Az] = map Atom ["nil", "x", "y", "z"]
(* This makes "a::b" the same as "Comp("cons", [a, b])" *)
infixr 4 :::
fun a:::b = comp "cons" [a,b]


(* The microKanren equivalent of the following Prolog:
   peano(z).
   pean(s(N)) :- peano(N). *)
fun peano n = n==Az
               || fresh (fn r => n==comp"s"[r]
                                  && peano r)
(* Note the eta-conversion (i.e. the "fn s => ... s" is necessary,
   to prevent the recursive call to "wait x" happening too
   soon. This wasn't necessary in peano, as the (fn r => ...) served
   the same purpose there. Equivalent of the following Prolog:
         wait(please_wait).
         wait(X) :- wait(X). *)
fun wait x = fn subs => (x==Atom"please_wait" || wait x) subs
(* append([],R, R).
   append([LH|LT], R, [LH|REST]) :- append(LT, R, REST) *)
fun append l r out =
    (l == A_nil && out==r)
        || freshn 3 (fn [lh, lt, rest] =>
                        l==lh:::lt
                         && out==lh:::rest
                         && append lt r rest)
val () =
    (print_results 10 "X==x" X (X==Ax);
     (* Expected output
        X==x
        x *)
     print_results 10 "X==x || X==y"
                   X (X==Ax || X==Ay);
     (* Expected output
        X==x || X==y
        x
        y *)
     print_results 10 "X==x && Y==X"
                   Y (X==Ax && Y==X);
     (* Expected output
        X==x && Y==X
        x *)

     print_results 3 "peano(N)" N (peano N);
     (* Expected output
        peano(N)
        z
        s(z)
        s(s(z)) *)

     print_results 3  "wait(N) || peano N"
                   N (wait N || peano N);
     (* Expected output
        wait(N) || peano N
        please_wait
        please_wait
        please_wait *)

     print_results 4 "wait N II peano N"
                   N (wait N II peano N);
     (* Expected output
        wait N II peano N
        please_wait
        z
        please_wait
        s(z) *)

     print_results 10 "append(L,R,[1,2,3])"
                   (comp""[L,R])
                   (append L R (A1:::A2:::A3:::A_nil))
     (* Expected output
        append(L,R,[1,2,3])
        (nil, cons(1, cons(2, cons(3, nil))))
        (cons(1, nil), cons(2, cons(3, nil)))
        (cons(1, cons(2, nil)), cons(3, nil))
        (cons(1, cons(2, cons(3, nil))), nil) *)
    )

(* Examples with cut (Prolog's !):
        a(4).
        a(x).
        b(3,x).
        b(1,7).
        c(A, B, C) :- a(A), b(B, _), !, a(D).
        c(A, _, B) :- b(A, B).
 *)
fun a a0 = a0==Ax || a0==Ax
fun b a0 a1 = (a0==A3 && a1==Ax) ||
                                 (a0==A1 && a1==A7)
fun c a0 a1 a2 =
    If (once (a a0 && fresh (fn v => b a1 v))
       ,fresh (fn d => a d)
       ,b a0 a2)
val () =
    (print_results 10 "c(P,Q,R)"
                   (comp"c"[P,Q,R]) (c P Q R);
     (* Expected output
        c(P,Q,R)
        c(x, 3, R)
        c(x, 3, R) *)
     print_results 10 "c(1,6,R)"
                   (comp"c"[A1,A6,R]) (c A1 A6 R)
     (* Expected output
        c(1,6,R)
        c(1, 6, 7) *)
    )
