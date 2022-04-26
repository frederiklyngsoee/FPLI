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
    | TRUE b            -> [Asm.IPUSH 1]
    | FALSE b           -> [Asm.IPUSH 0]
    | VAR x             -> [Asm.ILOAD (varpos x env)]
    | ADD (e1, e2)      -> comp env e1         @
                           comp ("" :: env) e2 @
                           [Asm.IADD]
    | LET (x, e1, e2)   -> comp env e1         @
                           comp (x :: env) e2  @
                           [Asm.ISWAP]         @
                           [Asm.IPOP]
    | EQEQ (e1, e2)       -> comp env e1       @
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
    | CALL (f,e)        -> comp env e          @
                           [Asm.ICALL f]       @
                           [Asm.ISWAP]         @
                           [Asm.IPOP]
    | OROR (e1, e2)     -> let _after = newLabel() 
                           let _fail = newLabel()
                           let _pass = newLabel()
                           comp env e1         @
                           [Asm.IPUSH 1]       @
                           [Asm.IEQ]           @
                           [Asm.IJMPIF _pass]  @
                           comp env e2         @
                           [Asm.IPUSH 1]       @
                           [Asm.IEQ]           @
                           [Asm.IJMPIF _pass]  @
                           [Asm.IJMP _fail]    @
                           [Asm.ILAB _pass]    @
                           [Asm.IPUSH 1]       @
                           [Asm.IJMP _after]   @
                           [Asm.ILAB _fail]    @
                           [Asm.IPUSH 0]       @
                           [Asm.ILAB _after]
    | ANDAND (e1, e2)   -> let _then = newLabel()  
                           let _after = newLabel()
                           let _fail = newLabel()
                           let _pass = newLabel()
                           comp env e1         @
                           [Asm.IPUSH 1]       @
                           [Asm.IEQ]           @
                           [Asm.IJMPIF _then]  @
                           [Asm.IJMP _fail]    @
                           [Asm.ILAB _then]    @
                           comp env e2         @
                           [Asm.IPUSH 1]       @
                           [Asm.IEQ]           @
                           [Asm.IJMPIF _pass]  @
                           [Asm.IJMP _fail]    @
                           [Asm.ILAB _pass]    @
                           [Asm.IPUSH 1]       @
                           [Asm.IJMP _after]   @
                           [Asm.ILAB _fail]    @
                           [Asm.IPUSH 0]       @
                           [Asm.ILAB _after]
    | LT (e1, e2)       -> comp env         e1 @     // Less Than Operator
                           comp ("" :: env) e2 @
                           [Asm.ILT]
    | LE (e1, e2)       -> let _then = newLabel()    // Less Than OR Equal Operator
                           let _after = newLabel()
                           comp env         e1     @
                           comp ("" :: env) e2     @
                           [Asm.ILT]               @
                           [Asm.IJMPIF _then]      @
                           comp env         e1     @
                           comp ("" :: env) e2     @
                           [Asm.IEQ]               @
                           [Asm.IJMP _after]       @
                           [Asm.ILAB _then]        @
                           [Asm.IPUSH 1]           @
                           [Asm.ILAB _after]
    | GE (e1, e2)       -> let _then = newLabel()     // Greather Than OR Equal Operator
                           let _after = newLabel()
                           comp env         e1     @
                           comp ("" :: env) e2     @
                           [Asm.ILT]               @
                           [Asm.IJMPIF _then]      @
                           [Asm.IPUSH 1]           @
                           [Asm.IJMP _after]       @
                           [Asm.ILAB   _then]      @
                           [Asm.IPUSH 0]           @
                           [Asm.ILAB   _after]
    | GT (e1, e2)       -> let _then1 = newLabel()    // Greater Than Operator
                           let _after1 = newLabel()
                           comp env         e1     @
                           comp ("" :: env) e2     @
                           [Asm.ILT]               @
                           [Asm.IJMPIF _then1]     @
                           comp env         e1     @
                           comp ("" :: env) e2     @
                           [Asm.IEQ]               @
                           [Asm.IJMPIF _then1]     @
                           [Asm.IPUSH 1]           @
                           [Asm.IJMP _after1]      @
                           [Asm.ILAB   _then1]     @
                           [Asm.IPUSH 0]           @
                           [Asm.ILAB   _after1]


let compile = function
    PROG (defs, e) -> comp [] e
