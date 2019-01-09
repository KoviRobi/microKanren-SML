# microKanren in Standard ML
For more details on microKanren see the
[original paper](http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf),
though I find the
[racket implementation](https://cgi.soic.indiana.edu/~c311/lib/exe/fetch.php?media=microkanren.pdf)
a bit clearer

Files:
- [microkanren.sml](microkanren.sml):
  The microKanren implementation, around 100 lines
  of Standard ML, and 150 lines of comments.
- [sequence.sml](sequence.sml):
   A simple lazy-list/stream/sequence library for
   Standard ML, necessary for [microkanren.sml](microkanren.sml).
- [microkanren_prettyprint.sml](microkanren_prettyprint.sml):
   Basic pretty-printers for terms, necessary for
   [microkanren_examples.sml](microkanren_examples.sml).
- [microkanren_examples.sm](microkanren_examples.sml):
   A couple of examples, with their corresponding
   Prolog implementation.

There is the branch [structures](../../tree/structures) which uses Standard
ML's signatures and structures, and might or might not be easier to
read. The benefit of that is you are able to see the types of
functions side-by-side with their implementations, by looking at both
the *.sig and *.sml file.

Running:
To run in PolyML, run
`poly --use microkanren.sml`, to run the examples, do `poly --use microkanren.sml --use microkanren_prettyprint.sml --use microkanren_examples.sml -q` (the `-q` makes PolyML quiet, and supresses printing of types, which makes the printed output stand on its own).

To run in SML/NJ, use `sml microkanren.sml` and `sml microkanren.sml microkanren_prettyprint.sml microkanren_examples.sml`.

Here is what the output should be (minus any types and warnings):
```
$ poly --use microkanren.sml --use microkanren_prettyprint.sml --use microkanren_examples.sml -q
X==x
x

X==x || X==y
x
y

X==x && Y==X
x

peano(N)
z
s(z)
s(s(z))

wait(N) || peano N
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
