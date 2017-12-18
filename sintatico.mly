%{
  open Lexing
  open Ast
  open Sast
%}

(* PALAVRAS RESERVADAS *)
%token <Lexing.position> BREAK
%token <Lexing.position> CASE
%token <Lexing.position> CHAR
%token <Lexing.position> DEFAULT
%token <Lexing.position> ELSE
%token <Lexing.position> IF
%token <Lexing.position> INT
%token <Lexing.position> FLOAT
%token <Lexing.position> FOR
%token <Lexing.position> RETURN
%token <Lexing.position> SWITCH
%token <Lexing.position> WHILE
%token <Lexing.position> VOID

(* GAMBIARRAS *)
%token <Lexing.position> PRINTF
%token <Lexing.position> SCANF
  	
(* LITERAIS *)
%token <int * Lexing.position> LITINT 
%token <float * Lexing.position> LITFLOAT 
%token <char * Lexing.position> LITCHAR 
%token <string * Lexing.position> LITSTRING 

(* OPERADORES ARITMETICOS *)
%token <Lexing.position> MAIS
%token <Lexing.position> MENOS
%token <Lexing.position> MULT
%token <Lexing.position> DIV
%token <Lexing.position> INC 
%token <Lexing.position> DEC

(* OPERADORES LOGICOS *)
%token <Lexing.position> AND
%token <Lexing.position> OR

(* OPERADORES RELACIONAIS *)
%token <Lexing.position> IGUAL
%token <Lexing.position> DIFERENTE
%token <Lexing.position> MAIOR
%token <Lexing.position> MENOR
%token <Lexing.position> MAIORIGUAL
%token <Lexing.position> MENORIGUAL 

(* SEPARADORES *)
%token <Lexing.position> PONTO
%token <Lexing.position> VIRG
%token <Lexing.position> PONTOEVIRG
%token <Lexing.position> DOISPONTOS
%token <Lexing.position> APAR
%token <Lexing.position> FPAR
%token <Lexing.position> ACHAVE
%token <Lexing.position> FCHAVE
%token <Lexing.position> ACOLCHETE
%token <Lexing.position> FCOLCHETE

(* OUTROS *)	
%token <Lexing.position> ATRIB
%token <Lexing.position> SHARP
%token <Lexing.position> INCLUDE
%token <Lexing.position> ECOMERCIAL
%token <string * Lexing.position> ID
%token EOF

(* ASSOCIACAO // MENOR PRECEDENCIA*)
%left OR
%left AND
%left IGUAL DIFERENTE
%left MAIOR MENOR MAIORIGUAL MENORIGUAL 
%left MAIS MENOS	
%left MULT DIV
(* ASSOCIACAO // MAIOR PRECEDENCIA*)

(* ===================================================================================================== *)
(* INICIO *)
(* ===================================================================================================== *)

%start <Sast.expressao Ast.programa> programa

%%

(* ===================================================================================================== *)

programa: 
  b=biblioteca* 
  d=declaracao_funcao*
  EOF					{ Programa(b, d) }

(* ===================================================================================================== *)

tipo:
  | t=tipo_simples  			{ t }
	

(* ===================================================================================================== *)

tipo_simples:
  | INT  				{ TipoInt }
  | FLOAT   				{ TipoFloat }
  | CHAR 				{ TipoChar }
  | VOID 				{ TipoVoid }

(* ===================================================================================================== *)

biblioteca: 
  SHARP 
  INCLUDE 
  MENOR
  id=ID
  PONTO 
  ID 
  MAIOR					{ Biblioteca(id) }
	
(* ===================================================================================================== *)

declaracao_variavel: 
  t=tipo 
  ids=separated_nonempty_list(VIRG, ID) 
  PONTOEVIRG 				{List.map (fun id -> DecVar(t,id)) ids }
	
(* ===================================================================================================== *)

declaracao_funcao: 
  tret=tipo 
  nome=ID 
  APAR 
  formais=separated_list(VIRG, parametro)
  FPAR 
  ACHAVE 
  ds=declaracao_variavel*
  cs=comando* 	
  FCHAVE 				{ DecFuncao {
  				            fn_nome = nome;
					    fn_tiporet = tret;
					    fn_formais = formais;
					    fn_locais = List.flatten ds;
					    fn_corpo = cs
				          }
     					}

(* ===================================================================================================== *)

parametro: t=tipo nome=ID  { (t, nome) }

(* ===================================================================================================== *)
(* COMANDOS *)
(* ===================================================================================================== *)

comando:  
  | c=comando_return PONTOEVIRG		{ c }
  | c=comando_atribuicao PONTOEVIRG	{ c }
  | c=comando_printf PONTOEVIRG		{ c }
  | c=comando_scanf PONTOEVIRG		{ c }
  | c=comando_if         		{ c }
  | c=comando_switch      		{ c } 
  | c=comando_for     			{ c } 
  | c=comando_while       		{ c }
  | c=comando_funcao PONTOEVIRG		{ c }
  | c=comando_case 			{ c }
  | c=comando_default 			{ c }

(*  ===================================================================================================== *)

comando_return: 
  RETURN 
  e=expressao? 				{ CmdReturn e } 

(* ===================================================================================================== *)

comando_atribuicao: 
  | e=expressao INC			{ CmdIncr(e) }
  | e=expressao DEC 			{ CmdDecr(e) }
  | esq=expressao ATRIB dir=expressao 	{ CmdAtrib(esq,dir) }
	
(* ===================================================================================================== *)

comando_if: 
  IF 
  APAR
  teste=expressao
  FPAR 
  ACHAVE 
  entao=comando* 
  FCHAVE
  senao=option( ELSE
                ACHAVE
		cs=comando* 
		FCHAVE 
		{cs}
	      )				{ CmdIf(teste,entao,senao) }
(*
===================================================================================================== *)

comando_printf: 
  PRINTF 
  APAR 
  args=separated_nonempty_list(VIRG, expressao) 
  FPAR		 			{ CmdPrintf(args) }

(* ===================================================================================================== *)

comando_scanf: 
  SCANF 
  APAR 
  args=separated_nonempty_list(VIRG, expressao) 
  FPAR	 				{ CmdScanf(args) }

(* ===================================================================================================== *)

comando_switch:	
  SWITCH 	
  APAR 
  e=expressao
  FPAR
  ACHAVE 
  c=comando_case* 
  d=comando_default 
  FCHAVE				{CmdSwitch(e,c,d)}

(* ===================================================================================================== *)

comando_case: 
  CASE 
  e=expressao 
  DOISPONTOS 
  c=comando*
  BREAK	
  PONTOEVIRG				{ Case(e,c) }


(* ===================================================================================================== *)

comando_default: 
  DEFAULT
  DOISPONTOS
  c=comando* 				{Default(c)}

(* ===================================================================================================== *)

comando_for:
  FOR 
  APAR 
  at1=comando_atribuicao 
  PONTOEVIRG 
  e=expressao 
  PONTOEVIRG 
  at2=comando_atribuicao 
  FPAR
  ACHAVE 
  cs=comando* 
  FCHAVE				{ CmdFor(at1,e,at2,cs) }

(* ===================================================================================================== *)

comando_while:
  WHILE 
  APAR 
  ex=expressao 
  FPAR 
  ACHAVE 
  cs=comando* 
  FCHAVE				{ CmdWhile(ex,cs) }

(* ===================================================================================================== *)

comando_funcao:
  exp=chamada PONTOEVIRG		{ CmdChamada exp }

(* ===================================================================================================== *)
(* EXPRESSOES *)
(* ===================================================================================================== *)

expressao: 
  | v=variavel    			{ ExpVar v }
  | ECOMERCIAL v= variavel 		{ ExpEnd v } 
  | i=LITINT				{ ExpInt i }
  | s=LITSTRING   			{ ExpString s } 
  | f=LITFLOAT    			{ ExpFloat f }
  | c=LITCHAR     			{ ExpChar c }
  | c = chamada				{ c }
  | e1=expressao op=oper e2=expressao 	{ ExpOp (op, e1, e2) }
  | APAR e=expressao FPAR 		{ e }
	
(*  ===================================================================================================== *)

chamada : 
  id=ID 
  APAR 
  args=separated_list(VIRG, expressao) 
  FPAR					{ ExpChamada(id,args) }

(*  ===================================================================================================== *)

%inline oper:
  | pos = MAIS  			{ (Mais, pos) }
  | pos = MENOS 			{ (Menos, pos) }
  | pos = MULT  			{ (Mult, pos) }
  | pos = DIV   			{ (Div, pos) }
  | pos = MAIOR 			{ (Maior, pos) }
  | pos = MENOR 			{ (Menor, pos) }
  | pos = MAIORIGUAL 			{ (MaiorIgual, pos) }
  | pos = MENORIGUAL 			{ (MenorIgual, pos) }
  | pos = IGUAL 			{ (Igual, pos) }
  | pos = DIFERENTE 			{ (Diferente, pos) }      
  | pos = AND 				{ (E, pos) }
  | pos = OR 				{ (Ou, pos) }

(* ===================================================================================================== *)

variavel: 
  | id=ID       			{ VarSimples id }
  | v=variavel
    ACOLCHETE 
    e=expressao
    FCOLCHETE 				{ VarElemento(v,e) }

