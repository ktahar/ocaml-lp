{
open Parser

let keywords = Hashtbl.create 32

let _ = List.iter (fun (k, v) ->
  Hashtbl.add keywords k v)
  [
    "end", END;
    "max", MAX;
    "maximize", MAX;
    "min", MIN;
    "minimize", MIN;
    "bound", BOUND;
    "bounds", BOUND;
    "st", ST;
    "generals", GENERAL;
    "general", GENERAL;
    "gen", GENERAL;
    "binaries", BINARY;
    "binary", BINARY;
    "bin", BINARY;
    "free", FREE;
    "infinity", INF;
    "inf", INF;
  ]
}

let digit = ['0'-'9']
let alphabet = ['a'-'z' 'A'-'Z' '_']
let number = (digit+ | (digit+ "." digit*) | (digit* "." digit+))
             (['e' 'E'] ['+' '-']? digit+)?
let id = alphabet (alphabet | digit)*

let square = '^' ' '* '2'
let div2 = '/' ' '* '2'
let l_bracket = ('+' ' '*)? '['
let white = ['\t' ' ' '\r' '\n' ']']

rule token = parse
  | '\\' { comment lexbuf; token lexbuf }
  (* keywords including non-alphabets *)
  | "subject to" | "such that" | "s.t."{ ST }
  (* symbols including white spaces *)
  | square { SQ }
  | l_bracket | div2 | white+ { token lexbuf }
  | number as n { NUM (float_of_string n ) }
  | id as str { (* ignore case to find keywords *)
      let s = String.lowercase_ascii str in
      match Hashtbl.find_opt keywords s with
      | Some kw -> kw
      | None ->
          ID str
  }
  | "+"  { PLUS }
  | "-"  { MINUS }
  | "*"  { TIMES }
  | ":"  { COLON }
  | "="  { EQ }
  | "<" | "<=" | "=<" { LT }
  | ">" | ">=" | "=>" { GT }
and comment = parse
  | ('\n' | eof) { () }
  | _ { comment lexbuf }
