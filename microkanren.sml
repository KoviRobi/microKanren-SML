(* This implements microKanren, in Standard ML. For more details on
   microKanren, see:

   (I think this one is slightly cleaner, as it mentions the types of functions)
   https://cgi.soic.indiana.edu/~c311/lib/exe/fetch.php?media=microkanren.pdf
   (This is the original)
   http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf

   This file is split into the following parts, delimited by ""
   (form-feed, Control-L, 0xC)
   1. Substitutions
   2. Unification
   3. Pure operators
   4. Impure operators
*)

(* For simplicity, atoms will only be strings, but if you wanted to,
   you could modify it to include other structures including
   lists. Personally, I feel this is a place where typeclasses like in
   Haskell would help make the implementation neater.

   Note that variables are either User supplied ones, or Internal
   ones. Internal variables make the implementation simpler, as we
   don't need to do variable renaming to avoid collisions of variables
   which have the same name, only keep an integer counter.
   *)
datatype term = Atom of string
              | Var of variable
              | Comp of string * term list
     and variable = User of string | Internal of int

(* Before implementing unification, we need to implement
   substitutions, which map variables to terms. We are using an
   association-list, which is just a list of (key,value) pairs. *)
type substitutions = (variable * term) list
val empty_subs = []

(* Note how we do the look-up recursively, this is needed for
   unification, for example in the following case
                substitutions: A -> B, B -> 1, C -> D, D -> 2
                unify A with D
   If we wouldn't do recursive look-up, we could end up unifying B
   with D, which would be wrong, as 1 and 2 should not unify. *)
fun lookup (subs : substitutions) (v as Var id) =
    (case List.find (fn (id', t) => id=id') subs of
         NONE => v
       | SOME (_, t) => lookup subs t)
  | lookup _ x = x
(* This isn't necessary for microKanren, but is useful for rewriting
   terms with variables in them for a given substitution.  *)
fun expand_term subs (Comp (n,ts)) =
    Comp (n, map (expand_term subs) ts)
  | expand_term subs (v as Var id) =
    (* Variable may map to a compound term,
       which in turn should be fully expanded *)
    (case List.find (fn (id', t) => id=id') subs of
         NONE => v
       | SOME (_, t) => expand_term subs t)
  | expand_term _ x = x
(* You can turn occurs_check on or off (it is off by default) by using
             occurs_check := true
   similarly to how you can do
             set_prolog_flag(occurs_check, true).
    in Prolog *)
val occurs_check = ref false
fun occurs id =
    fn Var id' => id=id'
  | Atom _ => false
  | Comp(_, ts) => List.exists (occurs id) ts

fun extend (subs : substitutions) id term =
    if !occurs_check andalso
       occurs id term then NONE
    else SOME ((id, term)::subs)


(* Part 2: Unification *)

(* This is similar to slide Basics/11, or bookwork question 2R.1,
   which is why it might be an extension on it. What is different is
   that we expand terms under the current substitution (this expansion
   just returns the supplied terms for non-variables). This is
   required for correctness, and the expansion must be full, that is
   it shouldn't expand to a variable which itself has a binding in the
   substitutions. Note the function type is
                  unify : substitution -> term * term -> substitution option
   and you might not have come across the 'a option type, it is either
   NONE or SOME of 'a. *)
fun unify subs (t1, t2) =
    let val t1' = lookup subs t1
        val t2' = lookup subs t2
    in if t1=t2 then SOME subs
       else case (t1', t2') of
                (Var id, t2') => extend subs id t2'
              | (t1', Var id) => extend subs id t1'
              | (Comp (n1, ts1), Comp (n2, ts2)) =>
                if n1<>n2 orelse length ts1 <> length ts2
                then NONE
                else foldl (fn (_, NONE) => NONE
                           | (ts, SOME s) => unify s ts)
                           (SOME subs)
                           (ListPair.zip (ts1, ts2))
              | (_, _) => NONE
    end


(* Part 3: Pure operators: unification (==), conjunction (&&),
   disjunction (||) and interleaving disjunction (II)

   For examples, see the file microkanren_examples.sml *)

(* You might not have come across the infix syntax before, it enables
   us to write
      A && B            instead of              &&(A, B)

    The integers are precedences, i.e.
        A==B && B==2 || C==1
   is the same as the following
        ((A==B) && (B==2)) || (C==1)

   Having && and ||, II not of the same precedence is not always the
   right thing to do (e.g. it causes problems in C as people aren't
   expecting it), but I reason that in this case it is a common
   pattern to have conjunctions (&&) in disjunctions (||, II),
   especially if we are thinking in Prolog terms, where a rule is a
   disjunction of clauses, and a clause is a conjunction of
   unifications and calls to other rules. *)
infixr 3 ==
infixr 2 &&
infixr 1 || II

(* Use statements require the file named by the string to be in the
   current directory. You can do this by:
   - You can do this in Linux by navigating to that directory in the
      terminal, then executing "poly" (or "sml", or your preferred
     implementation).
   - In Windows, you can right-click on the Poly shortcut, and modify
     the "Start in" to be where the "sequence.sml" file is located.
   - Alternatively, within your Standard ML implementation, you can use
     "OS.FileSys.chDir : string -> unit" to change directory, and
     "OS.FileSys.getDir : unit -> string" to find where you are in the
     first place. For more details, see
        http://sml-family.org/Basis/os-file-sys.html#OS_FILE_SYS:SIG:SPEC

   Note, that the semicolon seems necessary, to force SML to load the
   sequence.sml file first, then continue compiling the rest of this
   file.

   We will need a lazy-list library, which has the following:

    datatype 'a seq = consq of 'a * (unit -> 'a seq) | nilq
    val seq : 'a list -> 'a seq

    (* Note how the second argument to appq is a function, this is
    just convention here, but I use this convention as in some cases,
    it allows the second argument to be lazy *)
    val appq : 'a seq * (unit -> 'a seq) -> 'a seq

    (* mappq stands for map and append sequence, it can be implemented by
          fun mappq f seq = foldrq appq nilq (mapq f seq) *)
    val mappq : ('a -> 'b seq) -> 'a seq -> 'b seq

    (* interleaveq([a0, a1, ...], [b0, b1, ...]) = [a0, b0, a1, b1, ...] *)
    val interleaveq : 'a seq * (unit -> 'a seq) -> 'a seq
 *)
val () = use "sequence.sml";
(* The state represents one possible answer to a query (at the top
   level). It consists of a set of substitutions, and an integer,
   which is the first free Internal variable. *)
type state = substitutions * int

(* A goal represents a query, e.g. in Prolog
          true
          A=1 ; A=2
   A goal takes a state (at the top level, it is the start state), and
   returns a stream (lazy list, or sequence) of substitutions. *)
type goal = state -> state seq
val start_state = (empty_subs, 0)

(* The simplest goals, either succeed, not changing the unifications,
   or fail.
        val True : goal
        val False : goal *)
fun True state = seq [state]
fun False state = nilq

(* This is a wrapper around unifications, to make it work on goals. If
   the unification fails, returning the empty sequence is the same as
   saying the query cannot be satisfied. Else use the new
   subsitutions.
        val == : term * term -> goal *)
fun (t1==t2) (subs, free_var) =
    (case unify subs (t1,t2) of
         NONE => nilq
       | SOME subs' => seq [(subs', free_var)])

(* This isn't something that is visible to the user in Prolog, but it
   is necessary. Consider the following Prolog rules

        two_steps(X, Z) :- one_step(X, Y), one_step(Y, Z).
        head_is_foo([X|_]) :- foo(x).

   In the first rule, Y is an internal variable, thus it should be a
   fresh variable, e.g. we would write the following in this
   microKanren:

        fun two_steps x z =
            fresh (fn y => one_step x y && one_step y z)

   Here, using fresh variables for the arguments isn't necessary, as
   they would just be unified with the arguments.

   In the second rule, we need a fresh variable for X, to use it to
   deconstruct the input list, i.e.

        fun head_is_foo list =
            freshn 2 (fn [x, dont_care] =>
                         list==Comp("cons", [x, dont_care])
                         && foo x)


        val fresh : (term -> goal) -> goal
        val freshn : int -> (term list -> goal) -> goal *)
fun fresh f (subs, first_free) =
    f (Var(Internal first_free)) (subs, first_free+1)
(* Asking for multiple free-variables at a time. *)
fun freshn n f (subs, first_free) =
    let fun mkVar i = Var(Internal (first_free+i))
        val vars = List.tabulate (n, mkVar)
    in f vars (subs, first_free+n) end

(* Disjunction takes two goals, and append the resulting stream
               val || : goal * goal -> goal *)
fun (goal1 || goal2) state =
    appq (goal1 state, fn () => goal2 state)
(* Fair disjunction does round-robin on both of its goals
               val II : goal * goal -> goal *)
fun (goal1 II goal2) state =
    interleaveq (goal1 state, fn () => goal2 state)

(* Conjunction evaluates goal1 with the state, and map+appends the
   resulting sequence of states. It needs the append to flatten the
   result of "map goal2 (goal1 state) : state seq seq".
               val II : goal * goal -> goal *)
fun (goal1 && goal2) state =
    mappq goal2 (goal1 state)


(* Part 4: Impure operators: "If" and "once", these two allow us to
   implement Prolog's cut. To rewrite a rule which uses cut, use the
   "If" as the joiner of the two clauses (instead of ||) , and use
   "once" for anything before the cut (!) inside the clause. E.g.

        foo(0).
        foo(1) :- bar(1), !, bar(2).
        foo(2) :- bar(3).

   could be written here as

        fun foo x subst =
            (x == 0
             || If (once (x == 1 && bar 1))
                   , bar 2
                   , bar 3) subst
 *)

fun If (condition, true_case, false_case) state =
    case condition state of nilq => false_case state
                          | seq  => mappq true_case seq

fun once goal state =
    case goal state of nilq => nilq
                     | consq(x,xf) => seq [x]
