(* $Id: maindc.ml,v 1.5 2017-04-07 13:24:41-07 - - $ *)

include Scanner
include Bigint

open Bigint
open Printf
open Scanner

type stack_t = Bigint.bigint Stack.t
let push = Stack.push
let pop = Stack.pop
let ld_st = Hashtbl.create 10

let ord thechar = int_of_char thechar
type binop_t = bigint -> bigint -> bigint

let rec print_number number = 
    let strnum = string_of_bigint number in
        let length = String.length strnum in
            if length > 68
                then (printf "%s\\\n" (String.sub strnum 0 69);
                        print_number(bigint_of_string
                        (String.sub strnum 69 (length-69))))
            else printf "%s\n%!" (string_of_bigint number)

let print_stackempty () = printf "dc: stack empty\n%!"

let executereg (thestack: stack_t) (oper: char) (reg: int) =
    try match oper with
        | 'l' -> if (Hashtbl.mem ld_st reg)
                 then push (Hashtbl.find ld_st reg) thestack
            else printf "operator l reg 0%o is unimplemented\n%!" reg
        | 's' -> Hashtbl.replace ld_st reg (pop thestack)
        | _   -> printf "0%o 0%o is unimplemented\n%!" (ord oper) reg
    with Stack.Empty -> print_stackempty()

let executebinop (thestack: stack_t) (oper: binop_t) =
    try let right = pop thestack
        in  try let left = pop thestack
                in  push (oper left right) thestack
            with Stack.Empty -> (print_stackempty ();
                                 push right thestack)
    with Stack.Empty -> print_stackempty ()

let execute (thestack: stack_t) (oper: char) =
    try match oper with
        | '+'  -> executebinop thestack Bigint.add
        | '-'  -> executebinop thestack Bigint.sub
        | '*'  -> executebinop thestack Bigint.mul
        | '/'  -> executebinop thestack Bigint.div
        | '%'  -> executebinop thestack Bigint.rem
        | '^'  -> executebinop thestack Bigint.pow
        | 'c'  -> Stack.clear thestack
        | 'd'  -> push (Stack.top thestack) thestack
        | 'f'  -> Stack.iter print_number thestack
        | 'p'  -> print_number (Stack.top thestack)
        | 'q'  -> raise End_of_file
        | '\n' -> ()
        | ' '  -> ()
        | _    -> printf "0%o is unimplemented\n%!" (ord oper)
    with Stack.Empty -> print_stackempty()

let toploop (thestack: stack_t) inputchannel =
    let scanbuf = Lexing.from_channel inputchannel in
    let rec toploop () = 
        try  let nexttoken = Scanner.scanner scanbuf
             in  (match nexttoken with
                 | Number number       -> push number thestack
                 | Regoper (oper, reg) -> executereg thestack oper reg
                 | Operator oper       -> execute thestack oper
                 );
             toploop ()
        with End_of_file -> printf "";
    in  toploop ()

let readfiles () =
    let thestack : bigint Stack.t = Stack.create ()
    in  ((if Array.length Sys.argv > 1 
         then try  let thefile = open_in Sys.argv.(1)
                   in  toploop thestack thefile
              with Sys_error message -> (
                   printf "%s: %s\n%!" Sys.argv.(0) message;
                   exit 1));
        toploop thestack stdin)

let interact () =
    let thestack : bigint Stack.t = Stack.create ()
    in  toploop thestack stdin

let _ = if not !Sys.interactive then readfiles ()

