module Syntax

type name     = string

type exp    = INT   of int
            | NAME  of name
            | ADD   of exp * exp
            | LET   of name * exp * exp
            | IF    of exp * exp * exp
            | VAR   of name
            | EQ    of exp * exp
            | CALL  of name * exp
            | MULT  of exp * exp

            
type def    = FUNC  of name * (name * exp)


type prog   = PROG  of def list * exp  
