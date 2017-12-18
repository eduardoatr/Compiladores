open Ast

type expressao = ExpVar of (expressao variavel) * tipo
  | ExpEnd of (expressao variavel) * tipo
  | ExpInt of int * tipo
  | ExpString of string * tipo 
  | ExpFloat of float * tipo
  | ExpChar of char * tipo
  | ExpVoid
  | ExpChamada of ident * (expressao expressoes) * tipo
  | ExpOp of (oper * tipo) * (expressao * tipo) * (expressao * tipo)

