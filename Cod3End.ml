open Printf
open Ast
open Tast

(* ===================================================================================================== *)

type endereco =
   | Nome of string
   | ConstInt of int
   | ConstFloat of float
   | Temp of int
   | Null

(* ===================================================================================================== *)

and instrucao =
     AtribBin of endereco * endereco * opBin * endereco  (* x = y op z *)
   | AtribUn  of endereco * opUn * endereco              (* x = op y   *)
   | Copia of endereco * endereco                        (* x = y      *)
   | Goto of instrucao                                   (* goto L     *)
   | If of endereco *  instrucao                         (* if x goto L *)
   | IfFalse of endereco * instrucao                     (* ifFalse x goto L *)
   | IfRelgoto of endereco * opRel * endereco * instrucao 
                                                      (* if x oprel y goto L *)
   | Call of string * (endereco * Ast.tipo) list * Ast.tipo (* call p,[(x,t)],t *) 
   | Recebe of string * Ast.tipo
   | Local of string * Ast.tipo
   | CallFn of endereco * string *  (endereco * Ast.tipo) list * Ast.tipo   (* x = call p,n,t *)
   | Return of endereco
   | BeginFun of string * int * int      (* beginFun p,nparam, nlocais *)
   | EndFun
   | Rotulo of string 

(* ===================================================================================================== *)

and opBin = Ast.oper * Ast.tipo

and opUn = Ast.oper * Ast.tipo

and opRel = Ast.oper * Ast.tipo

(* ===================================================================================================== *)

let conta_temp = ref 0
let conta_rotulos = ref (Hashtbl.create 5)

(* ===================================================================================================== *)

let zera_contadores () =
  begin
    conta_temp := 0;
    conta_rotulos := Hashtbl.create 5
  end

(* ===================================================================================================== *)
  
let novo_temp () =
   let numero = !conta_temp in
   let _ = incr conta_temp in
   Temp numero

(* ===================================================================================================== *)
   
let novo_rotulo prefixo =
  if Hashtbl.mem !conta_rotulos prefixo
  then
     let numero = Hashtbl.find !conta_rotulos prefixo in
     let _ = Hashtbl.replace !conta_rotulos prefixo (numero + 1) in
     Rotulo (prefixo ^ (string_of_int numero))     
  else
     let _ = Hashtbl.add !conta_rotulos prefixo 1 in
     Rotulo (prefixo ^ "0")

(* ===================================================================================================== *)
(* Codigo para impressao *)
(* ===================================================================================================== *)

let endr_to_str = function
   | Nome s -> s
   | ConstInt n -> string_of_int n
   | ConstFloat n -> string_of_float n
   | Temp n  -> "t" ^ string_of_int n

(* ===================================================================================================== *)

let tipo_to_str t =
    match t with
      TipoInt 	-> "inteiro"
    | TipoChar 	-> "char"
    | TipoFloat -> "float"
    | TipoVoid	-> "void"

(* ===================================================================================================== *)

let op_to_str op = 
  match op with
  | Mais  -> "+"
  | Menos -> "-"
  | Mult  -> "*"
  | Div   -> "/"
  | Maior -> ">"
  | Menor -> "<"
  | Igual -> "="
  | MaiorIgual -> ">=" 
  | MenorIgual -> "<="
  | Diferente -> "!="
  | E     -> "&&"
  | Ou    -> "||"

(* ===================================================================================================== *)

let rec args_to_str ats =
   match ats with
   | [] -> ""
   | [(a,t)] -> 
     let str = sprintf "(%s,%s)" (endr_to_str a) (tipo_to_str t) in
     str
   | (a,t) :: ats -> 
     let str = sprintf "(%s,%s)" (endr_to_str a) (tipo_to_str t) in
     str ^ ", " ^ args_to_str ats

(* ===================================================================================================== *)
  
let rec escreve_cod3 c =
  match c with
  | AtribBin (x,y,op,z) -> 
      sprintf "%s := %s %s %s\n" (endr_to_str x) 
                                (endr_to_str y) (op_to_str (fst op)) (endr_to_str z)
  | AtribUn (x,op,y) ->
      sprintf "%s := %s %s\n" (endr_to_str x) (op_to_str (fst op)) (endr_to_str y)
  | Copia (x,y) ->
      sprintf "%s := %s\n" (endr_to_str x) (endr_to_str y)
  | Goto l ->
      sprintf "goto %s\n" (escreve_cod3 l)
  | If (x,l) -> 
      sprintf "if %s goto %s\n" (endr_to_str x) (escreve_cod3 l)
  | IfFalse (x,l) -> 
      sprintf "ifFalse %s goto %s\n" (endr_to_str x) (escreve_cod3 l)
  | IfRelgoto (x,oprel,y,l) -> 
      sprintf "if %s %s %s goto %s\n" (endr_to_str x) (op_to_str (fst oprel)) 
                                     (endr_to_str y) (escreve_cod3 l)
  | Call (p,ats,t) -> sprintf "call %s(%s): %s\n" p (args_to_str ats) (tipo_to_str t)
  | Recebe (x,t) -> sprintf "recebe %s,%s\n" x (tipo_to_str t)
  | Local (x,t) -> sprintf "local %s,%s\n" x (tipo_to_str t)
  | CallFn (x,p,ats,t) -> 
      sprintf "%s = call %s(%s): %s\n" (endr_to_str x) p (args_to_str ats) (tipo_to_str t)
  | Return (x) -> sprintf "return %s\n" (endr_to_str x)
  | BeginFun (id,np,nl) -> sprintf "beginFun %s,%d,%d\n" id np nl
  | EndFun -> "endFun\n\n"
  | Rotulo r -> sprintf "%s: " r

(* ===================================================================================================== *)

let rec escreve_codigo cod =
  match cod with
  | [] -> printf "\n"
  | c::cs -> printf "%s" (escreve_cod3 c); 
             escreve_codigo cs 

(* ===================================================================================================== *)
(* Código do tradutor para codigo de 3 enderecos *)
(* ===================================================================================================== *)

let pega_tipo exp = 
  match exp with
  | ExpInt (n, t) -> t
  | ExpFloat(n, t) -> t
  | ExpChar (n, t) -> t
  | ExpString (n, t) -> t
  | ExpVar (v, t) -> t
  | ExpEnd (v, t) -> t
  | ExpOp ((op,t),_,_) -> t
  | ExpChamada (id, args, t) -> t
  | _ -> failwith "pega_tipo: nao implementado"

(* ===================================================================================================== *)

let rec traduz_exp exp =
  match exp with
  | ExpInt (n, TipoInt) -> 
     let t = novo_temp () in
    (t, [Copia (t, ConstInt n)])

(* ===================================================================================================== *)

  | ExpFloat (n, TipoFloat) -> 
     let t = novo_temp () in
    (t, [Copia (t, ConstFloat n)])

(* ===================================================================================================== *)

  | ExpString (n, TipoChar) -> 
     let t = novo_temp () in
    (t, [Copia (t, Nome n)])

(* ===================================================================================================== *)

  | ExpVar (v, tipo) ->
    (match v with
       VarSimples nome -> let id = fst nome in ((Nome id), [])
     | _ -> failwith "traduz_exp: nao implementado"
    )

(* ===================================================================================================== *)

  | ExpOp (op, exp1, exp2) ->
    let (endr1, codigo1) = let (e1,t1) = exp1 in traduz_exp e1
    and (endr2, codigo2) = let (e2,t2) = exp2 in traduz_exp e2
    and t = novo_temp () in
    let codigo = codigo1 @ codigo2 @ [AtribBin (t, endr1, op, endr2)] in
    (t, codigo)  
  
(* ===================================================================================================== *)

  | ExpChamada (id, args, tipo_fn) ->
      let (enderecos, codigos) = List.split (List.map traduz_exp args) in
      let tipos = List.map pega_tipo args in
      let endr_tipos = List.combine enderecos tipos  
      and t = novo_temp () in 
      let codigo = (List.concat codigos) @
                   [CallFn (t, id, endr_tipos, tipo_fn)]
      in
        (t, codigo)
  | _ -> failwith "traduz_exp: nao implementado"

(* ===================================================================================================== *)

let rec traduz_cmd cmd =
  match cmd with
  | CmdReturn exp ->
    (match exp with
     | None -> [Return ( Null )]
     | Some e ->
       let (endr_exp, codigo_exp) = traduz_exp e in
       codigo_exp 
       @ [Return (endr_exp)]
    )

(* ===================================================================================================== *)

  | CmdAtrib (elem, exp) ->
    let (endr_exp, codigo_exp) = traduz_exp exp 
    and (endr_elem, codigo_elem) = traduz_exp elem in
    let codigo = codigo_exp @ codigo_elem @ [Copia (endr_elem, endr_exp)] 
    in codigo

(* ===================================================================================================== *)

  | CmdIf (teste, entao, senao) ->
    let (endr_teste, codigo_teste) = traduz_exp teste 
    and codigo_entao = traduz_cmds entao 
    and rotulo_falso = novo_rotulo "L" in
    (match senao with
        | None -> codigo_teste @ 
                  [IfFalse (endr_teste, rotulo_falso)] @
                  codigo_entao @ 
                  [rotulo_falso]
        | Some cmds -> 
          let codigo_senao = traduz_cmds cmds 
          and rotulo_fim = novo_rotulo "L" in
              codigo_teste @ 
              [IfFalse (endr_teste, rotulo_falso)] @
              codigo_entao @ 
              [Goto rotulo_fim] @
              [rotulo_falso] @ codigo_senao @
              [rotulo_fim]
    )

(* ===================================================================================================== *)

  | CmdWhile (teste, cmds) ->
    let (endr_teste, codigo_teste) = traduz_exp teste 
    and codigo_while = traduz_cmds cmds
    and rotulo_false = novo_rotulo "L" 
    and rotulo_top = novo_rotulo "L" in
      [rotulo_top] 
    @ codigo_teste 
    @ [IfFalse (endr_teste, rotulo_false)]
    @ codigo_while 
    @ [Goto rotulo_top]
    @ [rotulo_false]

(* ===================================================================================================== *)

  | CmdFor (atrib1, teste, atrib2, cmds) ->
    let (endr_teste, codigo_teste) = traduz_exp teste 
    and codigo_atrib1 = traduz_cmd atrib1
    and codigo_atrib2 = traduz_cmd atrib2
    and codigo_for = traduz_cmds cmds
    and rotulo_false = novo_rotulo "L" 
    and rotulo_top = novo_rotulo "L" in
      codigo_atrib1
    @ [rotulo_top] 
    @ codigo_teste 
    @ [IfFalse (endr_teste, rotulo_false)]
    @ codigo_for 
    @ codigo_atrib2
    @ [Goto rotulo_top]
    @ [rotulo_false] 

(* ===================================================================================================== *)

  | CmdSwitch (teste, cases, default) ->
    let (endr_teste, codigo_teste) = traduz_exp teste 
    and codigo_cases = traduz_cmds cases
    and codigo_default = traduz_cmd default in
      codigo_teste 
    @ codigo_cases 
    @ codigo_default   
    
(* ===================================================================================================== *)

  | Case (teste, cmds) ->
     let (endr_teste, codigo_teste) = traduz_exp teste 
     and rotulo_inicio = novo_rotulo "L"
     and codigo_cases = traduz_cmds cmds in
      codigo_teste
     @ [rotulo_inicio]
     @ codigo_cases

(* ===================================================================================================== *)

  | Default (cmds) ->
     let codigo_default = traduz_cmds cmds in
       codigo_default

(* ===================================================================================================== *)

  | CmdChamada (ExpChamada (id, args, tipo_fn)) -> 
      let (enderecos, codigos) = List.split (List.map traduz_exp args) in
      let tipos = List.map pega_tipo args in
      let endr_tipos = List.combine enderecos tipos in
      (List.concat codigos) @
      [Call (id, endr_tipos, tipo_fn)]

(* ===================================================================================================== *)

  | CmdPrintf args -> 
      let (enderecos, codigos) = List.split (List.map traduz_exp args) in
      let tipos = List.map pega_tipo args in
      let endr_tipos = List.combine enderecos tipos in
      (List.concat codigos) @
      [Call ("print", endr_tipos, TipoVoid)]

(* ===================================================================================================== *)

  | CmdScanf args -> 
      let (enderecos, codigos) = List.split (List.map traduz_exp args) in
      let tipos = List.map pega_tipo args in
      let endr_tipos = List.combine enderecos tipos in
      (List.concat codigos) @
      [Call ("read", endr_tipos, TipoVoid)]

(* ===================================================================================================== *)
    
  | _ -> failwith "traduz_cmd: nao implementado"
    
(* ===================================================================================================== *)

and traduz_cmds cmds =
  match cmds with
  | [] -> []
  | cmd :: cmds ->
     let codigo = traduz_cmd cmd in
     codigo @ traduz_cmds cmds

(* ===================================================================================================== *)

let traduz_fun ast =
  let trad_local x =
    match x with
      DecVar (t,(id,pos)) -> Local (id,t)
  in
  match ast with
    DecFuncao {fn_nome; fn_tiporet; fn_formais; fn_locais; fn_corpo} ->  
    let nome = fst fn_nome
    and formais = List.map (fun (tipo,(id,pos)) -> Recebe (id,tipo)) fn_formais
    and nformais = List.length fn_formais
    and locais = List.map trad_local fn_locais
    and nlocais = List.length fn_locais 
    and corpo = traduz_cmds fn_corpo 
    in
    [BeginFun (nome,nformais,nlocais)] @ formais @ locais @ corpo @ [EndFun]
        
(* ===================================================================================================== *)

let tradutor ast_tipada =
  let _ = zera_contadores () in 
  let (Programa (bibliotecas, decs_funs)) = ast_tipada in
  let funs_trad = List.map traduz_fun decs_funs in
  (List.concat funs_trad)

(* ===================================================================================================== *)

let teste ast =
   let _ = printf "\n" in
   escreve_codigo (tradutor ast)

