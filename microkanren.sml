(* This implements microKanren, in Standard ML. For more details on
   microKanren, see:

   (I think this one is slightly cleaner, as it mentions the types of functions)
   https://cgi.soic.indiana.edu/~c311/lib/exe/fetch.php?media=microkanren.pdf
   (This is the original)
   http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf

   This file is split into the following parts, delimited by ""
   (form-feed, Control-L, 0xC)
   1. Terms
   2. Substitutions
   3. Unification
   4. Pure operators
   5. Impure operators

   Each of these parts have their own signature in microkanren.sig
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

(* Part 1: Terms of the microKanren language. *)
structure Terms :> TERMS =
struct
datatype term = Atom of string
              | Var of variable
              | Comp of string * term list
     and variable = User of string | Internal of int
(* Pretty-printing *)
fun pp_term (Atom s) = s
  | pp_term (Var s) = pp_var s
  | pp_term (Comp (s, ts)) =
    s ^ "(" ^ (String.concatWith ", " (map pp_term ts)) ^ ")"
and pp_var (User s) = s
  | pp_var (Internal i) = "%"^Int.toString i
end


(* Part 2: Substitutions, mappings from variables to terms *)

structure Substitutions :> SUBSTITUTIONS
   where type term = Terms.term
     and type variable = Terms.variable =
struct
open Terms
(* Before implementing unification, we need to implement
   substitutions, which map variables to terms. We are using an
   association-list, which is just a list of (key,value) pairs. *)
type substitutions = (variable * term) list
val empty_subs = []
(* Pretty-printing *)
fun pp_substitutions s =
    "{ " ^ (String.concatWith ",\n, "
                              (map (fn (s, t) =>
                                       (pp_var s) ^
                                       " -> " ^
                                       (pp_term t))
                                   (rev s))) ^ "\n}"

(* Note how we do the look-up recursively, this is needed for
   unification, for example in the following case
                substitutions: A -> B, B -> 1, C -> D, D -> 2
                unify A with D
   If we wouldn't do recursive look-up, we could end up unifying B
   with D, which would be wrong, as 1 and 2 should not unify. *)
fun lookup subs (v as Var id) =
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

fun extend subs id term =
    if !occurs_check andalso
       occurs id term then NONE
    else SOME ((id, term)::subs)
end


(* Part 3: Unification *)
structure Unification :> UNIFICATION
   where type term = Terms.term
     and type substitutions = Substitutions.substitutions =
struct
open Terms
open Substitutions
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
    in if t1'=t2' then SOME subs (* Anything identical unifies *)
       else case (t1', t2') of
                (* Free variables unify (we have done lookup
                previously, extend may do occurs-check) *)
                (Var id, t2') => extend subs id t2'
              | (t1', Var id) => extend subs id t1'
                (* Compound terms unify if their top symbols match,
                 if their arities match, and they match recursively
                 (done by the fold) *)
              | (Comp (n1, ts1), Comp (n2, ts2)) =>
                if n1<>n2 orelse length ts1 <> length ts2
                then NONE
                else foldl (fn (_, NONE) => NONE
                             | (ts, SOME s) => unify s ts)
                           (SOME subs)
                           (ListPair.zip (ts1, ts2))
              | (_, _) => NONE
    end
end


(* Part 4: Pure operators: unification (==), conjunction (&&),
   disjunction (||) and interleaving disjunction (II)

   For examples, see the file microkanren_examples.sml *)
structure PureOperators :> PURE_OPERATORS
  where type 'a seq = 'a Sequence.seq
    and type term = Terms.term
    and type substitutions = Substitutions.substitutions =
struct
open Terms
open Sequence
open Substitutions
open Unification
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
   unifications and calls to other rules.

   Because Standard ML signatures don't allow exporting fixity (that
   an operator is infix), you will have to put these wherever you
   "open PureOperators" or use "PureOperators.==" *)
infixr 3 ==
infixr 2 &&
infixr 1 || II

(* The state represents one possible answer to a query (at the top
   level). It consists of a set of substitutions, and an integer,
   which is the first free Internal variable. *)
type state = substitutions * int
(* Pretty-printing *)
fun pp_state (subs, free_var) =
    "Used "^(Int.toString free_var)^" variables\n"^
    (pp_substitutions subs)

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
end


(* Part 5: Impure operators: "If" and "once", these two allow us to
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
structure ImpureOperators :> IMPURE_OPERATORS
   where type goal = PureOperators.goal =
struct
open Sequence
type goal = PureOperators.goal
fun If (condition, true_case, false_case) state =
    case condition state of nilq => false_case state
                          | seq  => mappq true_case seq

fun once goal state =
    case goal state of nilq => nilq
                     | consq(x,xf) => seq [x]
end
