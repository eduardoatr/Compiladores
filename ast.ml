(* The type of the abstract syntax tree (AST). *)

open Lexing

(* ===================================================================================================== *)

type ident = string
type 'a pos =  'a * Lexing.position (* tipo e posicao no arquivo fonte *)

(* ===================================================================================================== *)

type 'expr programa = Programa of biblioteca list * ('expr declaracao_funcao) list

(* ===================================================================================================== *)

and biblioteca = Biblioteca of ident pos

(* ===================================================================================================== *)

and 'expr declaracao_funcao = DecFuncao of ('expr decfn)

and 'expr decfn = {
  fn_nome:    ident pos;
  fn_tiporet: tipo;
  fn_formais: (tipo * ident pos) list;
  fn_locais:  declaracoes;
  fn_corpo:   'expr comandos
}

(* ===================================================================================================== *)
            
and tipo =
  | TipoInt
  | TipoFloat
  | TipoChar  
  | TipoVoid

(* ===================================================================================================== *)

and parametro = tipo * ident pos

(* ===================================================================================================== *)
  
and 'expr variaveis = ('expr variavel) list

(* ===================================================================================================== *)

and 'expr variavel = 
  | VarSimples of ident pos
  | VarElemento of ('expr variavel) * 'expr

(* ===================================================================================================== *)

and declaracao_variavel = DecVar of tipo * (ident pos)

(* ===================================================================================================== *)

and declaracoes = declaracao_variavel list

(* ===================================================================================================== *)	
(* COMANDOS *)
(* ===================================================================================================== *)

and 'expr comando =
  | CmdReturn of 'expr option
  | CmdAtrib of 'expr * 'expr
  | CmdPrintf of 'expr expressoes  
  | CmdScanf of 'expr expressoes
  | CmdIncr of 'expr
  | CmdDecr of 'expr
  | CmdIf of 'expr * ('expr comandos) * ('expr comandos option)
  | CmdSwitch of 'expr * ('expr comandos)  * ('expr comando) 
  | CmdFor of ('expr comando) * 'expr * ('expr comando) * ('expr comandos) 
  | CmdWhile of 'expr * ('expr comandos)
  | CmdChamada of 'expr
  | Case of 'expr * ('expr comandos)
  | Default of ('expr comandos)

(* ===================================================================================================== *)

and 'expr comandos = ('expr comando) list

(* ===================================================================================================== *)

and 'expr expressoes = 'expr list

(* ===================================================================================================== *)
 
and oper = 
  |Menos
  | Mais
  | Mult
  | Div
  | Maior
  | Menor
  | MaiorIgual 
  | MenorIgual 
  | Igual
  | Diferente
  | E
  | Ou	

