module Amb = AmbInterp
module A = Ast
module S = Sast
module T = Tast

(* ===================================================================================================== *)

exception Valor_de_retorno of T.expressao

(* ===================================================================================================== *)

let obtem_nome_tipo_var exp = let open T in
  match exp with
  | ExpEnd (v,tipo) ->
    (match v with
      | A.VarSimples (nome,_) -> (nome,tipo)
      | _ -> failwith "obtem_nome_tipo_var: nao implementado"
    )

  | ExpVar (v,tipo) ->
    (match v with
      | A.VarSimples (nome,_) -> (nome,tipo)
      | _ -> failwith "obtem_nome_tipo_var: nao implementado"
    )
  
  | _ -> failwith "obtem_nome_tipo_var: nao eh variavel"

(* ===================================================================================================== *)

let pega_int exp =
  match exp with
  |  T.ExpInt (i,_) -> i
  | _ -> failwith "pega_int: nao eh inteiro"

(* ===================================================================================================== *)

let compara a b =
   if(a > b) then 1 else 0;;

(* ===================================================================================================== *)

let compara2 a b =
   if(a >= b) then 1 else 0;;

(* ===================================================================================================== *)

let compara3 a b =
   if(a == b) then 1 else 0;;

(* ===================================================================================================== *)

let compara4 a b =
   if(a != b) then 1 else 0;;

(* ===================================================================================================== *)

let comparae a b =
   if(a && b) then 1 else 0;;

(* ===================================================================================================== *)

let comparaou a b =
   if(a || b) then 1 else 0;;

(* ===================================================================================================== *)

let pega_string exp =
  match exp with
  |  T.ExpString (s,_) -> s
  | _ -> failwith "pega_string: nao eh string"

(* ===================================================================================================== *)

let pega_char exp =
  match exp with
  |  T.ExpChar (c,_) -> c
  |  T.ExpString (s,_) -> s.[0]
  | _ -> failwith "pega_char: nao eh char"

(* ===================================================================================================== *)

let pega_float exp =
  match exp with
  |  T.ExpFloat (f,_) -> f
  | _ -> failwith "pega_float: nao eh float"

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

let rec interpreta_exp amb exp =
  let open A in
  let open T in
  match exp with
  | ExpVoid
  | ExpInt _
  | ExpString _
  | ExpFloat _
  | ExpChar _   -> exp

(* ===================================================================================================== *)
  | ExpEnd _	
  | ExpVar _ ->
    let (id,tipo) = obtem_nome_tipo_var exp in
    (* Tenta encontrar o valor da variável no escopo local, se não      *)
    (* encontrar, tenta novamente no escopo que engloba o atual. Prossegue-se *)
    (* assim até encontrar o valor em algum escopo englobante ou até    *)
    (* encontrar o escopo global. Se em algum lugar for encontrado,         *)
    (* devolve-se o valor. Em caso contrário, devolve uma exceção       *)
    (match (Amb.busca amb id) with
     | Amb.EntVar (tipo, v) ->
       (match v with
        | None -> failwith ("variável nao inicializada: " ^ id)
        | Some valor -> valor
       )
     |  _ -> failwith "interpreta_exp: expvar"
    )

(* ===================================================================================================== *)

  | ExpOp ((op,top), (esq, tesq), (dir,tdir)) ->
    let  vesq = interpreta_exp amb esq
    and vdir = interpreta_exp amb dir in

    let interpreta_aritmetico () =
      (match tesq with
	| TipoChar
	| TipoInt ->
	 (match op with
	  | Mais  ->     ExpInt(pega_int vesq + pega_int vdir, top)
	  | Menos -> ExpInt(pega_int vesq - pega_int vdir, top)
	  | Mult  ->     ExpInt(pega_int vesq * pega_int vdir, top)
	  | Div   ->      ExpInt(pega_int vesq / pega_int vdir, top)
	  | _     -> failwith "interpreta_aritmetico"
	 )
	| TipoFloat ->
         (match op with
          | Mais  ->     ExpFloat(pega_float vesq +. pega_float vdir, top)
          | Menos -> ExpFloat(pega_float vesq -. pega_float vdir, top)
          | Mult  ->      ExpFloat(pega_float vesq *. pega_float vdir, top)
          | Div   ->       ExpFloat(pega_float vesq /. pega_float vdir, top)
          | _     -> failwith "interpreta_aritmetico"
         )
       | _ -> failwith "interpreta_aritmetico"
      )

(* ===================================================================================================== *)

    and interpreta_relacional () =
      (match tesq with
       | TipoInt ->
         (match op with
          | Menor -> ExpInt(compara(pega_int vdir)(pega_int vesq), top)
          | Maior -> ExpInt(compara(pega_int vesq)(pega_int vdir), top)
          | MenorIgual -> ExpInt(compara2(pega_int vdir)(pega_int vesq), top)
	  | MaiorIgual -> ExpInt(compara2(pega_int vesq)(pega_int vdir), top)
 	  | Igual   -> ExpInt(compara3(pega_int vesq)(pega_int vdir), top)
          | Diferente   -> ExpInt(compara4(pega_int vesq)(pega_int vdir), top)
          | _ -> failwith "interpreta_relacional"
         )
       | TipoFloat ->
         (match op with
          | Menor -> ExpInt(compara(pega_float vdir)(pega_float vesq), top)
          | Maior -> ExpInt(compara(pega_float vesq)(pega_float vdir), top)
          | MenorIgual -> ExpInt(compara2(pega_float vdir)(pega_float vesq), top)
	  | MaiorIgual -> ExpInt(compara2(pega_float vesq)(pega_float vdir), top)
 	  | Igual   -> ExpInt(compara3(pega_float vesq)(pega_float vdir), top)
          | Diferente   -> ExpInt(compara4(pega_float vesq)(pega_float vdir), top)
          | _ -> failwith "interpreta_relacional"
         )
	| TipoChar ->
         (match op with
          | Menor -> ExpInt(compara(pega_char vdir)(pega_char vesq), top)
          | Maior -> ExpInt(compara(pega_char vesq)(pega_char vdir), top)
          | MenorIgual -> ExpInt(compara2(pega_char vdir)(pega_char vesq), top)
	  | MaiorIgual -> ExpInt(compara2(pega_char vesq)(pega_char vdir), top)
 	  | Igual   -> ExpInt(compara3(pega_char vesq)(pega_char vdir), top)
          | Diferente   -> ExpInt(compara4(pega_char vesq)(pega_char vdir), top)
          | _ -> failwith "interpreta_relacional"
         )
        | _ ->  failwith "interpreta_relacional"
       )

(* ===================================================================================================== *)
      
    and interpreta_logico () =
      (match tesq with
	| TipoInt ->
	 (match op with
	  | Ou -> ExpInt(1, top)
	  | E ->   ExpInt(1, top)
	  | _ ->  failwith "interpreta_logico"
	 )
	| TipoFloat ->
	 (match op with
	  | Ou -> ExpInt(1, top)
	  | E ->   ExpInt(1, top)
	  | _ ->  failwith "interpreta_logico"
	 )
	| TipoChar ->
	 (match op with
	  | Ou -> ExpInt(1, top)
	  | E ->   ExpInt(1, top)
	  | _ ->  failwith "interpreta_logico"
	 )
       	| _ ->  failwith "interpreta_logico"
       )    
    in
    let valor = (match (classifica op) with
          Aritmetico -> interpreta_aritmetico ()
        | Relacional -> interpreta_relacional ()
        | Logico -> interpreta_logico ()
      )
    in
      valor

(* ===================================================================================================== *)

 | ExpChamada (id, args, tipo) ->
    let open Amb in
    ( match (Amb.busca amb id) with
      | Amb.EntFun {tipo_fn; formais; locais; corpo} ->
           (* Interpreta cada um dos argumentos *)
           let vargs = List.map (interpreta_exp amb) args in
           (* Associa os argumentos aos parâmetros formais *)
           let vformais = List.map2 (fun (n,t) v -> (n, t, Some v)) formais vargs
           in interpreta_fun amb id vformais locais corpo
      | _ -> failwith "interpreta_exp: expchamada"
    )

(* ===================================================================================================== *)

and interpreta_fun amb fn_nome fn_formais fn_locais fn_corpo =
  let open A in
 (* Estende o ambiente global, adicionando um ambiente local *)
  let ambfn = Amb.novo_escopo amb in
   let insere_local  d =
    match d with
      (DecVar(t,v)) -> Amb.insere_local ambfn (fst v)  t None
  in
  (* Associa os argumentos aos parâmetros e insere no novo ambiente *)
  let insere_parametro(t,n,v) = Amb.insere_param ambfn n t v in
  let _ = List.iter insere_parametro fn_formais in
  (* Insere as variáveis locais no novo ambiente *)
    let _ = List.iter insere_local fn_locais in
    (* Interpreta cada comando presente no corpo da função usando o novo
       ambiente *)
  try
    let _ = List.iter (interpreta_cmd ambfn) fn_corpo in T.ExpVoid
    with
       Valor_de_retorno expret -> expret

(* ===================================================================================================== *)

and interpreta_cmd amb cmd =
  let open A in
  let open T in
  match cmd with
    CmdReturn exp ->
    (* Levantar uma exceção foi necessária pois, pela semântica do comando de
        retorno, sempre que ele for encontrado em uma função, a computação
        deve parar retornando o valor indicado, sem realizar os demais comandos.
    *)
    (match exp with
     (* Se a função não retornar nada, então retorne ExpVoid *)
       None -> raise (Valor_de_retorno ExpVoid)
     | Some e ->
       (* Avalia a expressão e retorne o resultado *)
       let e1 = interpreta_exp amb e in
       raise (Valor_de_retorno e1)
    )

(* ===================================================================================================== *)

  | CmdIf(teste, entao, senao) ->
    let teste1 = interpreta_exp amb teste in
    (match teste1 with
       ExpInt(1,_) ->
       (* Interpreta cada comando do bloco 'então' *)
       List.iter (interpreta_cmd amb) entao
     | _ ->
       (* Interpreta cada comando do bloco 'senão', se houver *)
       (match senao with
          None -> ()
        | Some bloco -> List.iter (interpreta_cmd amb) bloco
       )
    )

(* ===================================================================================================== *)

  | CmdAtrib(elem, exp) ->
    (* Interpreta o lado direito da atribuição *)
    let exp = interpreta_exp amb exp
    (* Faz o mesmo para o lado esquerdo *)
    and (elem1,tipo) = obtem_nome_tipo_var elem in
    Amb.atualiza_var amb elem1 tipo (Some exp)

(* ===================================================================================================== 

  | CmdSwitch(exp,case,def) ->
    let exp1 = interpreta_exp amb exp in
    let cases = List.iter (interpreta_cmd amb) case in
  
	
    let leia_var (nome,tipo) =	    
	let valor =
        (match (fst exp1) with
         | nome -> 
         | _ -> failwith "leia_var: nao implementado")
      in  Amb.atualiza_var amb nome tipo (Some valor)
    in
    (* Lê o valor para cada argumento e atualiza o ambiente *)
    List.iter leia_var nts
 ===================================================================================================== 

 | Case(exp,cmds) ->
   let exp1 = interpreta_exp amb exp in
   (try begin
	let resultado =
	(match (Amb.busca amb_global (fst exp1)) with
	 | EntVar  -> let aff = List.iter (interpreta_cmd amb) cmds in
	 | _ -> failwith "Variavel nao foi declarada" 
	)
     end with Not_found -> failwith "fucao main nao declarada")

 ===================================================================================================== 
*)

	| CmdIncr exp -> 
	    let exp = interpreta_exp amb exp
	    and (exp1, tipo) = obtem_nome_tipo_var exp in
	  Amb.atualiza_var amb exp1 tipo (Some exp)

(* ===================================================================================================== 
*)
	| CmdDecr exp -> 
	    let exp = interpreta_exp amb exp
	    and (exp1, tipo) = obtem_nome_tipo_var exp in
	  Amb.atualiza_var amb exp1 tipo (Some exp)

(* ===================================================================================================== *)

  | CmdChamada exp -> ignore( interpreta_exp amb exp)

(* ===================================================================================================== *)

 | CmdWhile (cond, cmds) ->
	let rec laco cond cmds =
		let condResp = interpreta_exp amb cond in
		    (match condResp with
			| ExpInt (1,_) ->
			  let _ = List.iter (interpreta_cmd amb) cmds in
			    laco cond cmds
		        | _ -> ()
		    )
	in laco cond cmds 

(* ===================================================================================================== *)

 | CmdFor (at1, cond, at2, cmds) ->
	let atinicial = interpreta_cmd amb at1 in
	let rec laco at1 cond at2 cmds =	      	
		let condResp = interpreta_exp amb cond in
		    (match condResp with
			| ExpInt (1,_) ->
			  			  let atinicial = interpreta_cmd amb at2 in
			  let _ = List.iter (interpreta_cmd amb) cmds in laco at1 cond at2 cmds 	 
        		| _ -> ()
		    )

	in laco at1 cond at2 cmds
	

(* ===================================================================================================== *)

  | CmdScanf exps ->
    (* Obtem os nomes e os tipos de cada um dos argumentos *)
    let nts = List.map (obtem_nome_tipo_var) exps in
    let leia_var (nome,tipo) =
      let valor =
        (match tipo with
         | A.TipoInt    -> T.ExpInt    (read_int (),  tipo)
         | A.TipoFloat   -> T.ExpFloat    (read_float (),  tipo)
         | A.TipoChar -> T.ExpString (read_line (), tipo)
         | _ -> failwith "leia_var: nao implementado"
        )
      in  Amb.atualiza_var amb nome tipo (Some valor)
    in
    (* Lê o valor para cada argumento e atualiza o ambiente *)
    List.iter leia_var nts

(* ===================================================================================================== *)

  | CmdPrintf exps ->
    (* Interpreta cada argumento da função 'saida' *)
    let exps = List.map (interpreta_exp amb) exps in
    let imprima exp =
      (match exp with
       | T.ExpInt (n,_) ->      let _ = print_int n in print_string " "
       | T.ExpFloat (f,_) ->      let _ = print_float f in print_string " "
       | T.ExpChar (c,_) ->      let _ = print_char c in print_string " "
       | T.ExpString (s,_) -> let _ = print_string s in print_string " "
       | _ -> failwith "imprima: nao implementado"
      )
    in
    let _ = List.iter imprima exps in
    print_newline ()

(* ===================================================================================================== *)

let insere_declaracao_var amb dec =
    match dec with
        A.DecVar(tipo, nome) ->  Amb.insere_local amb (fst nome) tipo None

(* ===================================================================================================== *)

let insere_declaracao_fun amb dec =
  let open A in
    match dec with
      DecFuncao {fn_nome; fn_tiporet; fn_formais; fn_locais; fn_corpo} ->
        let nome = fst fn_nome in
        let formais = List.map (fun (t,n) -> (t,(fst n))) fn_formais in
        Amb.insere_fun amb nome formais fn_locais fn_tiporet fn_corpo

(* ===================================================================================================== 

(* Lista de cabeçalhos das funções pré definidas *)
let fn_predefs = let open A in [
    ("entrada", [("x", TipoInt); ("y", TipoInt)], TipoVoid, []);
    ("saida",     [("x", TipoInt); ("y", TipoInt)], TipoVoid, []);
]

(* insere as funções pré definidas no ambiente global *)
let declara_predefinidas amb =
  List.iter (fun (n,ps,tr,c) -> Amb.insere_fun amb n ps [] tr c) fn_predefs

 ===================================================================================================== *)

let interprete ast =
  (* cria ambiente global inicialmente vazio *)
  let amb_global = Amb.novo_amb [] in
(*  let _ = declara_predefinidas amb_global in *)
  let (A.Programa(bibliotecas, decs_funs)) = ast in	
  let _ = List.iter (insere_declaracao_fun amb_global) decs_funs in
  (try begin
	(match (Amb.busca amb_global "main") with
	  | Amb.EntFun { tipo_fn ; formais; locais; corpo } ->	
	    let vformais = List.map (fun (t, n) -> (t, n, None)) formais in	
	    let _ = interpreta_fun amb_global "main" vformais locais corpo in()  	
	  | _ -> failwith "variavel declarada como 'main'")
	end with Not_found -> failwith "fucao main nao declarada")

	


