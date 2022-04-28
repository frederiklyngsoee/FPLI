module Parse

open FSharp.Text.Lexing

let fromString (s : string) = 
    let lexbuf = LexBuffer<char>.FromString s
    try
        Parser.start Lexer.tokenstream lexbuf
    with
      | exn -> failwith "Syntax error"

let fromFile (fileName : string) = 
    use textReader = new System.IO.StreamReader(fileName)
    let lexbuf = LexBuffer<char>.FromTextReader textReader
    try
        Parser.start Lexer.tokenstream lexbuf
    with
      | exn -> failwith "Syntax error"
