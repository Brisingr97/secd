(** An OCaml implementation of the original SECD machine *)
(** A Rational Deconstruction of Landin's SECD Machine (Olivier Danvy) *)
(** Section 2.1 *)
open AST

(** The type of values *)
type value =
  | INT of int
  | SUCC
  | CLOSURE of (env * ident * term)
(** The Environment register *)
and env = (ident * value) list
(** The Stack register *)
type stack = value list
(** The Control register *)
type instruction =
  | TERM of term
  | APPLY
type control = instruction list
(** The Dump register *)
type dump = (stack * env * control) list

(** The initial environment *)
let e_init = [("succ", SUCC)]

(** Lookup function for environment *)
let rec lookup id e =
  match e with
  | [] -> failwith "not found"
  | (id', v)::e' -> if id = id' then v else lookup id e'

(** The transition function *)
(** transition: stack * env * control * dump -> value *)
let rec transition = function
  | ([v], e', [], []) -> v
  | ([v], e', [], (s, e, c)::d) -> transition (v::s, e, c, d)
  | (s, e, (TERM (LIT n))::c, d) -> transition ((INT n)::s, e, c, d)
  | (s, e, (TERM (VAR x))::c, d) -> transition ((lookup x e)::s, e, c, d)
  | (s, e, (TERM (LAM (x, t)))::c, d) -> transition ((CLOSURE (e, x, t))::s, e, c, d)
  | (s, e, (TERM (APP (t0, t1)))::c, d) -> transition (s, e, (TERM t1)::(TERM t0)::APPLY::c, d)
  | (SUCC::(INT n)::s, e, APPLY::c, d) -> transition ((INT (n + 1))::s, e, c, d)
  | ((CLOSURE (e', x, t))::v'::s, e, APPLY::c, d) -> transition ([], (x, v')::e', [TERM t], (s, e, c)::d)
  | _ -> failwith "Stuck state"

let evaluate t = transition ([], e_init, [TERM t], [])
