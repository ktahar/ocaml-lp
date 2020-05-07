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
    "st", ST;
    "st.", ST;
    "s.t.", ST;
    "bound", BOUND;
    "bounds", BOUND;
    "gen", GENERAL;
    "general", GENERAL;
    "generals", GENERAL;
    "bin", BINARY;
    "binary", BINARY;
    "binaries", BINARY;
    "free", FREE;
    "infinity", INF;
    "inf", INF;
  ]

let kw_or_id str = (* ignore case to find keywords *)
  let s = String.lowercase_ascii str in
  match Hashtbl.find_opt keywords s with
  | Some kw -> kw
  | None ->
      ID str
}

let digit = ['0'-'9']
let alphabet = ['a'-'z' 'A'-'Z']
let symbol = ['_' '!' '#' '$' '%' '&' '(' ')' ',' '.' '?' '@' '{' '}' '~']
let number = (digit+ | (digit+ "." digit*) | (digit* "." digit+))
             (['e' 'E'] ['+' '-']? digit+)?
let id = (alphabet | '_') (alphabet | digit | symbol)*

let subj = ['s' 'S'] ['u' 'U'] ['b' 'B'] ['j' 'J'] ['e' 'E'] ['c' 'C'] ['t' 'T']
let such = ['s' 'S'] ['u' 'U'] ['c' 'C'] ['h' 'H']
let that = ['t' 'T'] ['h' 'H'] ['a' 'A'] ['t' 'T']
let st = subj ' '+ ['t' 'T'] ['o' 'O'] | such ' '+ that

let square = '^' ' '* '2'
let div2 = '/' ' '* '2'
let minus_lb = '-' ' '* '['
let plus_lb = ('+' ' '*)? '['
let white = ['\t' ' ' '\r' '\n']

rule token = parse
  | '\\' { comment lexbuf; token lexbuf }
  (* keywords and symbols including whitespaces *)
  | square { SQ }
  | st { ST }
  | minus_lb { MLB }
  (* eat up whitespaces and unnecessary symbols *)
  | div2 | plus_lb | ']' | white+ { token lexbuf }
  | number as n { NUM (float_of_string n ) }
  | id as s { kw_or_id s }
  | "+"  { PLUS }
  | "-"  { MINUS }
  | "*"  { TIMES }
  | ":"  { COLON }
  | "<" | "<=" | "=<" { LT }
  | ">" | ">=" | "=>" { GT }
  | "="  { EQ }
and comment = parse
  | ('\n' | eof) { () }
  | _ { comment lexbuf }
