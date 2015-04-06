(** Abstract Syntax Tree of the source language *)

type ident = string
type term =
  | LIT of int
  | VAR of ident
  | LAM of (ident * term)
  | APP of (term * term)
type program = term
