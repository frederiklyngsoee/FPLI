module Syntax

type name   = string

type typ    = TINT
            | TFUN of typ * typ

type exp    = INT    of int
            | NAME   of name
            | ADD    of exp * exp
            | LET    of name * exp * exp
            | IF     of exp * exp * exp
            | VAR    of name
            | EQEQ   of exp * exp
            | CALL   of name * exp
            | MULT   of exp * exp
            | OROR   of exp * exp
            | ANDAND of exp * exp
            | GT     of exp * exp
            | GE     of exp * exp
            | LT     of exp * exp
            | LE     of exp * exp
            | ABS    of name * typ * exp
            | APP    of exp * exp
            | TRUE              
            | FALSE
            | READ 
            | WRITE  of exp

            
type def    = FUNC  of name * (name * exp)


type prog   = PROG  of def list * exp  
