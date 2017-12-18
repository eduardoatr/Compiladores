open Ast

type expressao = ExpVar of (expressao variavel)
  | ExpEnd of (expressao variavel) 
  | ExpInt of int pos
  | ExpString of string pos 
  | ExpFloat of float pos
  | ExpChar of char pos 
  | ExpChamada of ident pos * (expressao expressoes)
  | ExpOp of oper pos * expressao * expressao

