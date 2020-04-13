{
open Parser
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
  | "end" { END }
  | "maximize" | "max" { MAX }
  | "minimize" | "min" { MIN }
  | "subject to" | "such that" | "s.t." | "st" { ST }
  | "bounds" | "bound" { BOUND }
  | "generals" | "general" | "gen" { GENERAL }
  | "binaries" | "binary" | "bin" { BINARY }
  | "free" { FREE }
  | square { SQ }
  | l_bracket | div2 | white+ { token lexbuf }
  | id as str { ID str }
  | number as n { NUM (float_of_string n ) }
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
