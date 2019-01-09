# microKanren in Standard ML
For more details on microKanren see the
[original paper](http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf),
though I find the
[racket implementation](https://cgi.soic.indiana.edu/~c311/lib/exe/fetch.php?media=microkanren.pdf)
a bit clearer

Files:
- [microkanren.sig](microkanren.sig):
  The signatures of all the structures in
  [microkanren.sml](microkanren.sml).
- [microkanren.sml](microkanren.sml):
  The microKanren implementation, around 100 lines
  of Standard ML, and 150 lines of comments. Probably helpful to read
  [microkanren.sig](microkanren.sig) alongside this.
- [sequence.sig](sequence.sig):
  The signature which [sequence.sml](sequence.sml) implements.
- [sequence.sml](sequence.sml):
   A simple lazy-list/stream/sequence library for
   Standard ML, necessary for [microkanren.sml](microkanren.sml).
- [microkanren_examples.sm](microkanren_examples.sml):
   A couple of examples, with their corresponding
   Prolog implementation in the comments.

Running:
To run in PolyML, run
`poly --use sequence.sig --use microkanren.sig --use sequence.sml --use microkanren.sml`, to run the examples, do `poly --use sequence.sig --use microkanren.sig --use sequence.sml --use microkanren.sml --use microkanren_examples.sml` (the `-q` makes PolyML quiet, and supresses printing of types, which makes the printed output stand on its own).

To run in SML/NJ, use `sml sequence.sig microkanren.sig sequence.sml microkanren.sml` and `sml sequence.sig microkanren.sig sequence.sml microkanren.sml microkanren_examples.sml`.

Here is what the output should be (minus any types and warnings):
```
$ poly --use sequence.sig \
    --use microkanren.sig \
    --use sequence.sml \
    --use microkanren.sml \
    --use microkanren_examples.sml

X==x
x

X==x || X==y
x
y

X==x && Y==X
x

peano N
z
s(z)
s(s(z))

waitN || peano N
please_wait
please_wait
please_wait

wait N II peano N
please_wait
z
please_wait
s(z)

append(L,R,[1,2,3])
(nil, cons(1, cons(2, cons(3, nil))))
(cons(1, nil), cons(2, cons(3, nil)))
(cons(1, cons(2, nil)), cons(3, nil))
(cons(1, cons(2, cons(3, nil))), nil)

c(P,Q,R)
c(x, 3, R)
c(x, 3, R)

c(1,6,R)
c(1, 6, 7)
```

Happy hacking!
