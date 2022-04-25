module Comp
open Syntax

// Compute index of variable in environment
let rec varpos x = function
  | []       -> failwith ("unbound: " + x)
  | y :: env -> if x = y then 0 else 1 + varpos x env

// Generate a new label
let mutable labelCounter = 0
let newLabel _ =
  let this = labelCounter
  labelCounter <- this + 1;
  "L" + string(this)

// Compiler
let rec comp env = function
    | INT i             -> [Asm.IPUSH i]
    | VAR x             -> [Asm.ILOAD (varpos x env)]
    | ADD (e1, e2)      -> comp env e1         @
                           comp ("" :: env) e2 @
                           [Asm.IADD]
    | LET (x, e1, e2)   -> comp env e1         @
                           comp (x :: env) e2  @
                           [Asm.ISWAP]         @
                           [Asm.IPOP]
    | EQ (e1, e2)       -> comp env e1         @
                           comp ("" :: env) e2 @
                           [Asm.IEQ]
    | IF (e1, e2, e3)   -> let _then  = newLabel()
                           let _after = newLabel()
                           comp env e1         @
                           [Asm.IJMPIF _then]  @
                           comp env e3         @
                           [Asm.IJMP   _after] @
                           [Asm.ILAB   _then]  @
                           comp env e2         @
                           [Asm.ILAB _after]
    | MULT (e1, e2)     -> comp env e1         @
                           comp ("" :: env) e2 @
                           [Asm.IMUL]
    | CALL x            -> [NEEEEEEEEEEEEEJ]
    
let compile = function
    PROG (defs, e) -> comp [] e
