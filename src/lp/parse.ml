module I = Parser.MenhirInterpreter

let fail lexbuf (_ : Lpfile.section list I.checkpoint) =
  let invalid_char = Lexing.lexeme_char lexbuf 0 in
  let pos = lexbuf.lex_start_p in
  Printf.fprintf stderr "line %d column %d: syntax error near '%c'.\n%!"
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)
    invalid_char ;
  None

let loop lexbuf result =
  let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  let succeed sections = Some sections in
  I.loop_handle succeed (fail lexbuf) supplier result

let process lexbuf =
  try loop lexbuf (Parser.Incremental.sections lexbuf.lex_curr_p)
  with Lexer.Error (c, pos) ->
    Printf.fprintf stderr "line %d column %d: invalid symbol '%c'.\n%!"
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)
      c ;
    None

let emit lexbuf =
  match process lexbuf with
  | Some secs ->
      Lpfile.emit secs
  | None ->
      failwith "Failed to parse LP file format."

let from_string s = emit @@ Lexing.from_string s

let from_file file =
  let ch = open_in file in
  try
    let problem = emit @@ Lexing.from_channel ch in
    close_in ch ; problem
  with e -> close_in_noerr ch ; raise e
