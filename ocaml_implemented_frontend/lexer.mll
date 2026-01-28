{
  open Parser
  open Lexing

  exception SyntaxError of string

  let get_pos lexbuf =
    let pos = lexbuf.lex_curr_p in
    Printf.sprintf "%s:%d:%d" 
      pos.pos_fname 
      pos.pos_lnum 
      (pos.pos_cnum - pos.pos_bol + 1)

  let unescape_char s =
    match s with
    | "\\n" -> '\n'
    | "\\t" -> '\t'
    | "\\r" -> '\r'
    | "\\\\" -> '\\'
    | "\\'" -> '\''
    | "\\\"" -> '"'
    | _ -> failwith "Invalid escape sequence"
}

(* Regular expressions *)
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let alphanum = alpha | digit | '_'

(* Identifiers *)
let lowercase_id = ['a'-'z'] alphanum*
let uppercase_id = ['A'-'Z'] alphanum*

(* Numbers *)
let integer = '-'? digit+
let float_num = '-'? digit+ '.' digit+ ('e' '-'? digit+)?
let hex_string = "0x" ['0'-'9' 'a'-'f' 'A'-'F']+
let bin_string = "0b" ['0' '1']+
let oct_string = "0o" ['0'-'7']+

(* Strings and chars *)
let escape_seq = '\\' ['n' 't' 'r' '\\' '\'' '"']

rule token = parse
  (* Whitespace *)
  | whitespace    { token lexbuf }
  | newline       { new_line lexbuf; token lexbuf }
  
  (* Comments *)
  | "//"          { line_comment lexbuf }
  | "/{" { block_comment 0 lexbuf }
  | "!/{" { doc_comment lexbuf }
  
  (* Keywords *)
  | "func"        { FUNC }
  | "proc"        { PROC }
  | "data"        { DATA }
  | "otherwise"   { OTHERWISE }
  | "Von"         { VON }
  | "mod"         { MOD }
  | "app"         { APP }
  | "class"       { CLASS }
  | "instance"    { INSTANCE }
  | "if"          { IF }
  | "else"        { ELSE }
  | "use"         { USE }
  | "otherwise"   { OTHERWISE }
  | "true" | "t"  { TRUE }
  | "false" | "f" { FALSE }
  | "in"          { IN }
  | "super"       { SUPER }
  
  (* Directives *)
  | "@using"      { AT_USING }
  | "@start"      { AT_START }
  | "@output"     { AT_OUTPUT }
  | "@root"       { AT_ROOT }
  
  (* Type keywords *)
  | "Int"         { TINT }
  | "Uint"        { TUINT }
  | "Float"       { TFLOAT }
  | "Bool"        { TBOOL }
  | "Char"        { TCHAR }
  | "UniChar"     { TUNICHAR }
  | "String"      { TSTRING }
  | "UniString"   { TUNISTRING }
  | "Nil"         { TNIL }
  | "I8"          { TI8 }
  | "I16"         { TI16 }
  | "I32"         { TI32 }
  | "I64"         { TI64 }
  | "I128"        { TI128 }
  | "U8"          { TU8 }
  | "U16"         { TU16 }
  | "U32"         { TU32 }
  | "U64"         { TU64 }
  | "U128"        { TU128 }
  | "F32"         { TF32 }
  | "F64"         { TF64 }
  
  (* Fixity *)
  | "infix"       { INFIX }
  | "infixl"      { INFIXL }
  | "infixr"      { INFIXR }
  
  (* Operators *)
  | "+"           { PLUS }
  | "-"           { MINUS }
  | "*"           { STAR }
  | "/"           { SLASH }
  | "^"           { CARET }
  | "_/_"         { FLOOR_DIV }
  | "&&"          { AND }
  | "||"          { OR }
  | "!"           { NOT }
  | "=="          { EQ }
  | "/="          { NEQ }
  | "<"           { LT }
  | ">"           { GT }
  | "<="          { LEQ }
  | ">="          { GEQ }
  | ".&"          { BIT_AND }
  | ".|"          { BIT_OR }
  | ".^"          { BIT_XOR }
  | ".~"          { BIT_NOT }
  | ":"           { CONS }
  | "++"          { CONCAT }
  | "."           { DOT }
  | "@"           { AT }
  
  (* Delimiters *)
  | "("           { LPAREN }
  | ")"           { RPAREN }
  | "{"           { LBRACE }
  | "}"           { RBRACE }
  | "["           { LBRACK }
  | "]"           { RBRACK }
  | "#{"          { SET_OPEN }
  | ","           { COMMA }
  | ";"           { SEMI }
  | "|"           { PIPE }
  | "=>"          { ARROW }
  | "->"          { TYPE_ARROW }
  | "="           { ASSIGN }
  | ":="          { INFER_ASSIGN }
  | "_"           { UNDERSCORE }
  | "\\"          { LAMBDA }
  | "..."         { ELLIPSIS }
  | "...="        { RANGE_INCLUSIVE }
  
  (* Literals *)
  | integer as i  { INT (int_of_string i) }
  | float_num as f { FLOAT (float_of_string f) }
  | hex_string as h { HEX_STRING h }
  | bin_string as b { BIN_STRING b }
  | oct_string as o { OCT_STRING o }
  
  (* String literals *)
  | '"'           { read_string (Buffer.create 17) lexbuf }
  | "\"0#"        { read_uni_string (Buffer.create 17) lexbuf }
  
  (* Char literals *)
  | '\'' ([^ '\\' '\''] as c) '\'' { CHAR c }
  | '\'' (escape_seq as s) '\''    { CHAR (unescape_char s) }
  
  (* Identifiers *)
  | lowercase_id as id { LOWER_ID id }
  | uppercase_id as id { UPPER_ID id }
  
  (* Custom operators - any sequence of special chars *)
  | (['!' '$' '%' '&' '*' '+' '-' '.' '/' '<' '=' '>' '?' '@' '^' '|' '~']+ as op)
    { CUSTOM_OP op }
  
  (* End of file *)
  | eof           { EOF }
  
  (* Error *)
  | _ as c        { raise (SyntaxError (Printf.sprintf "%s: Unexpected character: %c" (get_pos lexbuf) c)) }

and line_comment = parse
  | newline       { new_line lexbuf; token lexbuf }
  | eof           { EOF }
  | _             { line_comment lexbuf }

and block_comment depth = parse
  | "/{" { block_comment (depth + 1) lexbuf }
  | "}/" { if depth = 0 then token lexbuf else block_comment (depth - 1) lexbuf }
  | newline { new_line lexbuf; block_comment depth lexbuf }
  | eof { raise (SyntaxError "Unclosed block comment") }
  | _ { block_comment depth lexbuf }

and doc_comment = parse
  | "}/"          { token lexbuf }
  | newline       { new_line lexbuf; doc_comment lexbuf }
  | eof           { raise (SyntaxError "Unclosed doc comment") }
  | _             { doc_comment lexbuf }

and read_string buf = parse
  | '"'           { STRING (Buffer.contents buf) }
  | '\\' 'n'      { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 't'      { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' 'r'      { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' '\\'     { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' '"'      { Buffer.add_char buf '"'; read_string buf lexbuf }
  | newline       { new_line lexbuf; Buffer.add_char buf '\n'; read_string buf lexbuf }
  | eof           { raise (SyntaxError "Unclosed string") }
  | _ as c        { Buffer.add_char buf c; read_string buf lexbuf }

and read_uni_string buf = parse
  | '"'           { UNI_STRING (Buffer.contents buf) }
  | '\\' 'n'      { Buffer.add_char buf '\n'; read_uni_string buf lexbuf }
  | '\\' 't'      { Buffer.add_char buf '\t'; read_uni_string buf lexbuf }
  | '\\' 'r'      { Buffer.add_char buf '\r'; read_uni_string buf lexbuf }
  | '\\' '\\'     { Buffer.add_char buf '\\'; read_uni_string buf lexbuf }
  | '\\' '"'      { Buffer.add_char buf '"'; read_uni_string buf lexbuf }
  | newline       { new_line lexbuf; Buffer.add_char buf '\n'; read_uni_string buf lexbuf }
  | eof           { raise (SyntaxError "Unclosed unicode string") }
  | _ as c        { Buffer.add_char buf c; read_uni_string buf lexbuf }

{
}
