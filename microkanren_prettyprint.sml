(* Pretty-printing *)
fun pp_term (Atom s) = s
  | pp_term (Var s) = pp_var s
  | pp_term (Comp (s, ts)) =
    s ^ "(" ^ (String.concatWith ", " (map pp_term ts)) ^ ")"
and pp_var (User s) = s
  | pp_var (Internal i) = "%"^Int.toString i

fun pp_substitutions (s : substitutions) =
    "{ " ^ (String.concatWith ",\n, "
                              (map (fn (s, t) =>
                                       (pp_var s) ^
                                       " -> " ^
                                       (pp_term t))
                                   (rev s))) ^ "\n}"

fun pp_state (subs, free_var) =
    "Used "^(Int.toString free_var)^" variables\n"^
    (pp_substitutions subs)

(* These are helper functions for microkanren_examples.sml *)
fun print_term term (subs, _) =
    print (pp_term (expand_term subs term)^"\n")
fun run n goal =
    listq (takeq n (goal start_state))
fun print_results n str term goal =
    (print (str^"\n");
     map (print_term term) (run n goal);
     print "\n")
