signature TERMS =
sig
    datatype term = Atom of string
                  | Var of variable
                  | Comp of string * term list
         and variable = User of string | Internal of int
    val pp_term : term -> string
    val pp_var : variable -> string
end

signature SUBSTITUTIONS =
sig
    type term
    type variable
    type substitutions
    val pp_substitutions : substitutions -> string

    val empty_subs : substitutions
    val lookup : substitutions -> term -> term
    val expand_term : substitutions -> term -> term
    val occurs_check : bool ref
    val extend : substitutions ->
                 variable -> term -> substitutions option
end

signature UNIFICATION =
sig type term
    type substitutions
    val unify : substitutions
                -> term * term -> substitutions option
end

signature PURE_OPERATORS =
sig
    type 'a seq
    type term
    type substitutions
    (* Substitutions and first free internal variable *)
    type state = substitutions * int
    val start_state : state
    val pp_state : state -> string
    type goal = state -> state seq
    (* The simplest goal, always succeeds *)
    val True : goal
    val False : goal
    val == : term * term -> goal
    (* fresh f : Create a fresh (not equal to any existing) variable,
    and call f with the variables *)
    val fresh : (term -> goal) -> goal
    (* freshn n f : Create n many fresh (not equal to any existing)
    variables, and call f with the variables as a list *)
    val freshn : int -> (term list -> goal) -> goal
    val || : goal * goal -> goal
    val II : goal * goal -> goal
    val && : goal * goal -> goal
end

signature IMPURE_OPERATORS =
sig type goal
    (* COND -> TRUE_CASE ; FALSE_CASE *)
    val If : goal * goal * goal -> goal
    (* cut (!) *)
    val once : goal -> goal
end
