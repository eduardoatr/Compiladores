type entrada_fn = {
  tipo_fn:  Ast.tipo;
  formais: (Ast.tipo * string) list;
  locais:  Ast.declaracoes;
  corpo: Tast.expressao Ast.comandos
}

type entrada =
  |  EntFun of entrada_fn
  |  EntVar of Ast.tipo *  (Tast.expressao option)

type t

val novo_amb :  (string * entrada) list -> t
val novo_escopo : t -> t
val busca:              t -> string -> entrada
val atualiza_var:    t -> string -> Ast.tipo ->  (Tast.expressao option) -> unit
val insere_local :    t -> string -> Ast.tipo -> (Tast.expressao option) -> unit
val insere_param : t -> string -> Ast.tipo -> (Tast.expressao option) -> unit
val insere_fun :  t ->
  string ->
  (Ast.tipo * string) list ->
   Ast.declaracoes ->
  Ast.tipo -> (Tast.expressao Ast.comandos) -> unit
