{
  open Lexing
  open Printf
  open Sintatico

  exception Erro of string

  let incr_num_linha lexbuf =
    let pos = lexbuf.lex_curr_p in
      lexbuf.lex_curr_p <-
        { pos with pos_lnum = pos.pos_lnum + 1;
          pos_bol = pos.pos_cnum
        }

  let pos_atual lexbuf = lexbuf.lex_start_p

}

(* ===================================================================================================== *)

let digito = ['0' - '9']
let inteiro = '-'? digito+
let decimal = inteiro '.' inteiro

let letra = ['a' - 'z' 'A' - 'Z']
let identificador = letra ( letra | digito | '_')*

let brancos = [' ' '\t']+
let novalinha = '\r' | '\n' | "\r\n"

let comentario = "//" [^ '\r' '\n' ]*

(* ===================================================================================================== *)

rule token = parse
  | brancos { token lexbuf }
  | novalinha  { incr_num_linha lexbuf; token lexbuf }
  | comentario { token lexbuf }
  | "/*"       { comentario_bloco 0 lexbuf }

(* PALAVRAS RESERVADAS *)
  | "break"		{ BREAK (pos_atual lexbuf) }
  | "case"		{ CASE (pos_atual lexbuf) }
  | "char"		{ CHAR (pos_atual lexbuf) }
  | "default"		{ DEFAULT (pos_atual lexbuf) }
  | "else"		{ ELSE (pos_atual lexbuf) }
  | "float"		{ FLOAT (pos_atual lexbuf) }
  | "for"		{ FOR (pos_atual lexbuf) }
  | "if"       		{ IF (pos_atual lexbuf) }
  | "int"		{ INT (pos_atual lexbuf) }
  | "return" 		{ RETURN (pos_atual lexbuf) }
  | "switch" 		{ SWITCH (pos_atual lexbuf) }
  | "while"    		{ WHILE (pos_atual lexbuf) }

(* GAMBIARRAS *)
  | "printf"    	{ PRINTF (pos_atual lexbuf) }
  | "scanf"    		{ SCANF (pos_atual lexbuf) }

(* OPERADORES ARITMETICOS *)
  | '+'	     		{ MAIS (pos_atual lexbuf) }
  | '-' 		{ MENOS (pos_atual lexbuf) }
  | '*' 		{ MULT (pos_atual lexbuf) }	
  | '/' 		{ DIV (pos_atual lexbuf) }
  | "++"		{ INC (pos_atual lexbuf) }
  | "--"		{ DEC (pos_atual lexbuf) }

(* OPERADORES LOGICOS *)
  | "&&"		{ AND (pos_atual lexbuf) }
  | "||"		{ OR (pos_atual lexbuf) }

(* OPERADORES RELACIONAIS *)	
  | "==" 		{ IGUAL (pos_atual lexbuf) }
  | "!="		{ DIFERENTE (pos_atual lexbuf) }
  | ">"			{ MAIOR (pos_atual lexbuf) }
  | "<"			{ MENOR (pos_atual lexbuf) }
  | ">="		{ MAIORIGUAL (pos_atual lexbuf) }
  | "<="		{ MENORIGUAL (pos_atual lexbuf) }

(* SEPARADORES *)
  | "("        		{ APAR (pos_atual lexbuf) }
  | ")"        		{ FPAR (pos_atual lexbuf) }
  | "."			{ PONTO (pos_atual lexbuf) }
  | "," 		{ VIRG (pos_atual lexbuf) }
  | ";"			{ PONTOEVIRG (pos_atual lexbuf) }
  | ":"			{ DOISPONTOS (pos_atual lexbuf) }
  | "{"			{ ACHAVE (pos_atual lexbuf) }
  | "}"			{ FCHAVE (pos_atual lexbuf) }
  | "["			{ ACOLCHETE (pos_atual lexbuf) }
  | "]"			{ FCOLCHETE (pos_atual lexbuf) }

(* OUTROS *)	
  | "="       		{ ATRIB (pos_atual lexbuf) }
  | "void"		{ VOID (pos_atual lexbuf) }
  | "#"          	{ SHARP (pos_atual lexbuf) }
  | "include"    	{ INCLUDE (pos_atual lexbuf) }	
  | "&"			{ ECOMERCIAL (pos_atual lexbuf) }	
  | '"'  		{ let buffer = Buffer.create 1 in 
   		  	  let str = leia_string buffer lexbuf in
       		  	  LITSTRING (str, pos_atual lexbuf) } 
  | '''    		{ let pos = lexbuf.lex_curr_p in
		  	  let lin = pos.pos_lnum
		  	  and col = pos.pos_cnum - pos.pos_bol - 1 in
		  	  let buffer = Buffer.create 1 in 
		  	  let ctr = leia_char lin col buffer lexbuf in
		  	  LITCHAR (ctr, pos_atual lexbuf) }							  
  | identificador as x  { ID (x, pos_atual lexbuf) }
  | inteiro as n  	{ LITINT (int_of_string n, pos_atual lexbuf) }
  | decimal as n  	{ LITFLOAT (float_of_string n, pos_atual lexbuf) }
  | _  			{ raise (Erro ("Caracter desconhecido: " ^ Lexing.lexeme lexbuf)) }
  | eof  		{ EOF }

(* ===================================================================================================== *)		
	
and comentario_bloco n = parse
  | "*/"    		{ if n=0 then token lexbuf else comentario_bloco (n-1) lexbuf }
  | "/*"    		{ comentario_bloco (n+1) lexbuf }
  | _       		{ comentario_bloco n lexbuf }
  | eof     		{ raise (Erro "Comentario nao terminado") }

(* ===================================================================================================== *)

and leia_char lin col buffer = parse
  | '''      		{ Buffer.nth buffer 0}
  | _ as c    		{ Buffer.add_char buffer c; leia_char lin col buffer lexbuf }
  | eof       		{ raise (Erro "String nao foi fechada")}

(* ===================================================================================================== *)

and leia_string buffer = parse
  | '"'    		{ Buffer.contents buffer}
  | "\\t"     		{ Buffer.add_char buffer '\t'; leia_string buffer lexbuf }
  | "\\n"    		{ Buffer.add_char buffer '\n'; leia_string buffer lexbuf }
  | '\\' '"'  		{ Buffer.add_char buffer '"'; leia_string buffer lexbuf }
  | '\\' '\\' 		{ Buffer.add_char buffer '\\'; leia_string buffer lexbuf }
  | _ as c    		{ Buffer.add_char buffer c; leia_string buffer lexbuf }
  | eof       		{ raise (Erro "A string nao foi terminada") }	

