module Amb = Ambiente
module A = Ast
module S = Sast
module T = Tast

(* ===================================================================================================== *)

let rec posicao exp = let open S in
  match exp with
    | ExpVar v -> (match v with
                     | A.VarSimples (_,pos) -> pos
    		     | A.VarElemento (_,exp2) -> posicao exp2 )
    | ExpEnd v -> (match v with
    		     | A.VarSimples (_,pos) -> pos
    		     | A.VarElemento (_,exp2) -> posicao exp2 )
    | ExpInt (_,pos) -> pos
    | ExpString (_,pos) -> pos
    | ExpFloat (_,pos) -> pos
    | ExpChar (_,pos) -> pos
    | ExpOp ((_,pos),_,_)  -> pos
    | ExpChamada ((_,pos), _) -> pos

(* ===================================================================================================== *)

type classe_op = Aritmetico | Relacional | Logico

(* ===================================================================================================== *)

let classifica op = let open A in
  match op with
    | Ou
    | E  -> Logico
    | Menor
    | Maior
    | Igual
    | MenorIgual
    | MaiorIgual
    | Diferente -> Relacional
    | Mais
    | Menos
    | Mult
    | Div -> Aritmetico

(* ===================================================================================================== *)

let msg_erro_pos pos msg = let open Lexing in
  let lin = pos.pos_lnum
  and col = pos.pos_cnum - pos.pos_bol - 1 in
  Printf.sprintf "Semantico -> linha %d, coluna %d: %s" lin col msg

(* ===================================================================================================== *)

let msg_erro nome msg =
  let pos = snd nome in
  msg_erro_pos pos msg

(* ===================================================================================================== *)

let nome_tipo t = let open A in
  match t with
    | TipoInt 	-> "inteiro"
    | TipoChar 	-> "char"
    | TipoFloat -> "float"
    | TipoVoid 	-> "void"

(* ===================================================================================================== *)

let mesmo_tipo pos msg tinf tdec =
  if tinf <> tdec
    then
      let msg = Printf.sprintf msg (nome_tipo tinf) (nome_tipo tdec) in
      failwith (msg_erro_pos pos msg)

(* ===================================================================================================== *)

let rec infere_exp amb exp =
  match exp with
	| S.ExpInt n    -> (T.ExpInt (fst n, A.TipoInt),       A.TipoInt)
	| S.ExpFloat f  -> (T.ExpFloat (fst f, A.TipoFloat),     A.TipoFloat)
	| S.ExpChar c   -> (T.ExpChar (fst c, A.TipoChar),     A.TipoChar)
	| S.ExpString s -> (T.ExpString (fst s, A.TipoChar),     A.TipoChar)

(* ===================================================================================================== *)

	| S.ExpEnd v 	-> 
		(match v with
		A.VarSimples nome ->
		let id = fst nome in
		 (try (match (Amb.busca amb id) with
		       | Amb.EntVar tipo -> (T.ExpEnd (A.VarSimples nome, tipo), tipo)
		       | Amb.EntFun _ ->
			 let msg = "nome de funcao usado como nome de variavel: " ^ id in
			  failwith (msg_erro nome msg)
		     )
		  with Not_found ->
			 let msg = "A variavel " ^ id ^ " nao foi declarada" in
			 failwith (msg_erro nome msg)
		 )
	| _ -> failwith "infere_exp: nao implementado" )

(* ===================================================================================================== *)

	| S.ExpVar v ->
		(match v with
		A.VarSimples nome ->
		let id = fst nome in
		 (try (match (Amb.busca amb id) with
		       | Amb.EntVar tipo -> (T.ExpVar (A.VarSimples nome, tipo), tipo)
		       | Amb.EntFun _ ->
			 let msg = "nome de funcao usado como nome de variavel: " ^ id in
			  failwith (msg_erro nome msg)
		     )
		  with Not_found ->
			 let msg = "A variavel " ^ id ^ " nao foi declarada" in
			 failwith (msg_erro nome msg)
		 )
	| _ -> failwith "infere_exp: nao implementado" )

(* ===================================================================================================== *)

  	| S.ExpOp (op, esq, dir) ->
	    let (esq, tesq) = infere_exp amb esq
	    and (dir, tdir) = infere_exp amb dir in

	    let verifica_aritmetico () =
		      (match tesq with
			 A.TipoInt
		       | A.TipoChar
		       | A.TipoFloat ->
			 let _ = mesmo_tipo (snd op)
				      "O operando esquerdo eh do tipo %s mas o direito eh do tipo %s"
				      tesq tdir
			 in tesq (* O tipo da expressao aritmetica como um todo *)

		       | t -> let msg = "um operador aritmetico nao pode ser usado com o tipo " ^
				        (nome_tipo t)
			 in failwith (msg_erro_pos (snd op) msg) )

	    and verifica_relacional () =
		      (match tesq with
			 A.TipoInt
		       | A.TipoChar
		       | A.TipoFloat ->
			 let _ = mesmo_tipo (snd op)
				   "O operando esquerdo eh do tipo %s mas o direito eh do tipo %s"
				   tesq tdir
			 in tesq

		       | t -> let msg = "um operador relacional nao pode ser usado com o tipo " ^
				        (nome_tipo t)
			 in failwith (msg_erro_pos (snd op) msg) )

	    and verifica_logico () =
		      (match tesq with
			 A.TipoInt
		       | A.TipoChar
		       | A.TipoFloat ->
			 let _ = mesmo_tipo (snd op)
				   "O operando esquerdo eh do tipo %s mas o direito eh do tipo %s"
				   tesq tdir
			 in tesq

		       | t -> let msg = "um operador logico nao pode ser usado com o tipo " ^
				        (nome_tipo t)
			      in failwith (msg_erro_pos (snd op) msg) )
    
	    in
	    let op = fst op in
	    let tinf = (match (classifica op) with
		  Aritmetico -> verifica_aritmetico ()
		| Relacional -> verifica_relacional ()
		| Logico -> verifica_logico () )
	    in
	      (T.ExpOp((op,tinf), (esq, tesq), (dir, tdir)), tinf)

(* ===================================================================================================== *)

  	| S.ExpChamada (nome, args) ->
	     let rec verifica_parametros ags ps fs =
		match (ags, ps, fs) with
		 (a::ags), (p::ps), (f::fs) ->
		    let _ = mesmo_tipo (posicao a)
		             "O parametro eh do tipo %s mas deveria ser do tipo %s" p f
		    in verifica_parametros ags ps fs
	       | [], [], [] -> ()
	       | _ -> failwith (msg_erro nome "Numero incorreto de parametros")
	     in
	     let id = fst nome in
	     try
	       begin
		 let open Amb in

		 match (Amb.busca amb id) with
		 (* verifica se 'nome' está associada a uma funçao *)
		 | Amb.EntFun {tipo_fn; formais} ->
			 (* Infere o tipo de cada um dos argumentos *)
			 let argst = List.map (infere_exp amb) args
			 (* Obtem o tipo de cada parâmetro formal *)
			 and tipos_formais = List.map fst formais in
			 (* Verifica se o tipo de cada argumento confere com o tipo declarado *)
			 (* do parâmetro formal correspondente.                               *)
			 let _ = verifica_parametros args (List.map snd argst) tipos_formais
			 	in (T.ExpChamada(id, (List.map fst argst), tipo_fn), tipo_fn)
		 (* Se estiver associada a uma variável, falhe *)
		 | Amb.EntVar _ ->
			 let msg = id ^ " eh uma variavel e nao uma funcao" in
			 	failwith (msg_erro nome msg)
	       end
	      with Not_found ->
		       let msg = "Nao existe a funcao de nome " ^ id in
		       failwith (msg_erro nome msg)

(* ===================================================================================================== *)

let rec verifica_cmd amb tiporet cmd = let open A in
  match cmd with
     	| CmdReturn exp -> 
	     (match exp with
	     (* Se a funçao nao retornar nada, verifica se ela foi declarada como void *)
	     | None ->
		     let _ = mesmo_tipo (Lexing.dummy_pos)
				   "O tipo retornado eh %s mas foi declarado como %s"
				   TipoVoid tiporet
		     in CmdReturn None
	     | Some e ->
		     (* Verifica se o tipo inferido para a expressao de retorno confere com o *)
		     (* tipo declarado para a funçao.                                         *)
		     let (e1,tinf) = infere_exp amb e in
		     let _ = mesmo_tipo (posicao e)
		                    "O tipo retornado eh %s mas foi declarado como %s"
		                    tinf tiporet in
	  CmdReturn(Some e1) )

(* ===================================================================================================== *)

  	| CmdIf (teste, entao, senao) ->
	   let (teste1,tinf) = infere_exp amb teste in
	   (* Verifica a validade de cada comando do bloco 'entao' *)
	   let entao1 = List.map (verifica_cmd amb tiporet) entao in
	   (* Verifica a validade de cada comando do bloco 'senao', se houver *)
	   let senao1 =
		match senao with
		| None -> None
		| Some bloco -> Some (List.map (verifica_cmd amb tiporet) bloco) in
	  CmdIf(teste1, entao1, senao1)

(* ===================================================================================================== *)

	| CmdAtrib (elem, exp) ->
	    (* Infere o tipo da expressao no lado direito da atribuiçao *)
	    let (exp,  tdir) = infere_exp amb exp
	    (* Faz o mesmo para o lado esquerdo *)
	    and (elem1, tesq) = infere_exp amb elem in
	    (* Os dois tipos devem ser iguais *)
	    let _ = mesmo_tipo (posicao elem)
			       "Atribuicao com tipos diferentes: %s = %s" tesq tdir in
	  CmdAtrib(elem1, exp)

(* ===================================================================================================== *)

	| CmdIncr exp -> 
	    let (exp,tinf) = infere_exp amb exp in
	  CmdIncr exp

(* ===================================================================================================== *)

	| CmdDecr exp -> 
	    let (exp,tinf) = infere_exp amb exp in 
	  CmdDecr exp

(* ===================================================================================================== *)

	| CmdWhile (exp, cs) ->
	   let (exp1,tinf) = infere_exp amb exp in
	   (* Verifica a validade de cada comando do bloco 'entao' *)
	   let cs1 = List.map (verifica_cmd amb tiporet) cs in
	  CmdWhile(exp1, cs1)

(* ===================================================================================================== *)

	| CmdSwitch (exp, case, default) ->
	   let (exp1,tinf) = infere_exp amb exp in
	   (* Verifica a validade de cada comando do bloco 'entao' *)
	   let case1 = List.map (verifica_cmd amb tiporet) case in
	   let default1 = verifica_cmd amb tiporet default in 
	  CmdSwitch(exp1, case1, default1)

(* ===================================================================================================== *)

	| Case (exp, cs) ->
	   let (exp1,tinf) = infere_exp amb exp in
	   let cs1 = List.map (verifica_cmd amb tiporet) cs in
	  Case(exp1, cs1)

(* ===================================================================================================== *)

	| Default cs ->
	   let cs1 = List.map (verifica_cmd amb tiporet) cs in 
	  Default (cs1)

(* ===================================================================================================== *)

	| CmdFor (atrib1, exp, atrib2, cs) ->
	    let(atrib3) = verifica_cmd amb tiporet atrib1 in
	    let(exp1,tinf) = infere_exp amb exp in 
	    let(atrib4) = verifica_cmd amb tiporet atrib2 in
	    let cs1 = List.map (verifica_cmd amb tiporet) cs in
    	  CmdFor(atrib3, exp1, atrib4, cs1)

(* ===================================================================================================== *)

	| CmdPrintf exps ->
	    (* Verifica o tipo de cada argumento da funcao 'printf' *)
	    let exps = List.map (infere_exp amb) exps in
    	  CmdPrintf(List.map fst exps)

(* ===================================================================================================== *)

	| CmdScanf exps ->
	    (* Verifica o tipo de cada argumento da funcao 'scanf' *)
	    let exps = List.map (infere_exp amb) exps in
	  CmdScanf(List.map fst exps)

(* ===================================================================================================== *)

	| CmdChamada exp ->
	    let (exp,tinf) = infere_exp amb exp in
     	  CmdChamada exp

(* ===================================================================================================== *)

and verifica_fun amb ast = let open A in
  match ast with
    A.DecFuncao {fn_nome; fn_tiporet; fn_formais; fn_locais; fn_corpo} ->
    (* Estende o ambiente global, adicionando um ambiente local *)
    let ambfn = Amb.novo_escopo amb in
    (* Insere os parâmetros no novo ambiente *)
    let insere_parametro (t,v) = Amb.insere_param ambfn (fst v) t in
    let _ = List.iter insere_parametro fn_formais in
    (* Insere as variáveis locais no novo ambiente *)
    let insere_local = function
        (DecVar (t,v)) -> Amb.insere_local ambfn (fst v) t in
    let _ = List.iter insere_local fn_locais in
    (* Verifica cada comando presente no corpo da funcao usando o novo ambiente *)
    let corpo_tipado = List.map (verifica_cmd ambfn fn_tiporet) fn_corpo in
      A.DecFuncao {fn_nome; fn_tiporet; fn_formais; fn_locais; fn_corpo = corpo_tipado}

(* ===================================================================================================== *)

let rec verifica_dup xs =
  match xs with
    [] -> []
  | (t,nome)::xs ->
    let id = fst nome in
    if (List.for_all (fun (t,n) -> (fst n) <> id) xs)
    then (t, id) :: verifica_dup xs
    else let msg = "Parametro duplicado " ^ id in
      failwith (msg_erro nome msg)

(* ===================================================================================================== *)

let insere_declaracao_var amb dec =
  let open A in
    match dec with
        DecVar (tipo, nome) ->  Amb.insere_local amb (fst nome) tipo

(* ===================================================================================================== *)

let insere_declaracao_fun amb dec =
  let open A in
    match dec with
      DecFuncao {fn_nome; fn_tiporet; fn_formais; fn_corpo} ->
        (* Verifica se nao ha parametros duplicados *)
        let formais = verifica_dup fn_formais in
        let nome = fst fn_nome in
        Amb.insere_fun amb nome formais fn_tiporet

(* ===================================================================================================== *)

(* Lista de cabecalhos das funcoes pre definidas 
let fn_predefs = let open A in [
   ("printf", [(TipoChar,"x");(TipoInt,"y")] , TipoVoid);
]

(* ===================================================================================================== *)

 insere as funcoes pre definidas no ambiente global 
let declara_predefinidas amb =
  List.iter (fun (n,ps,tr) -> Amb.insere_fun amb n ps tr) fn_predefs
*)

(* ===================================================================================================== *)

let semantico ast =
  (* cria ambiente global inicialmente vazio *)
  let amb_global = Amb.novo_amb [] in
  (* let _ = declara_predefinidas amb_global in *) 
  let (A.Programa (bibliotecas, decs_funs)) = ast in
  let _ = List.iter (insere_declaracao_fun amb_global) decs_funs in
  (* Verificacao de tipos nas funcoes *)
  let decs_funs = List.map (verifica_fun amb_global) decs_funs in
  (* Verificacao de tipos na funcao principal *)
     (A.Programa (bibliotecas, decs_funs),  amb_global)
