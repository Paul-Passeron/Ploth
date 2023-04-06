type intrinsic =
  | Print
  | Dup
  | Swap
  | Rot
  | Drop
  | Over;;

type keyword = 
  | If
  | Else
  | End;;

type arithmetic =
  | Plus
  | Mult
  | Minus
  | Divmod
  | Max;;

type comparator =
  | Geq
  | Leq
  | G
  | L
  | Eq
  | Neq;;

type gate =
  | And
  | Or
  | Not;;

type prog_token = 
  | NoOp
  | Intrinsic of intrinsic
  | Arithmetic of arithmetic
  | Comparator of comparator
  | Gate of gate
  | Keyword of keyword
  | Int of int
  | Bool of bool;;

type 'a stack =
  | Empty
  | Node of 'a * 'a stack;;

let human (t: prog_token): string = match t with
  | Int(_) -> "`integer`"
  | Bool(_) -> "`bool` type"
  | Arithmetic(Plus) -> "`plus` arithmetic"
  | Arithmetic(Minus) -> "`minus` arithmetic"
  | Arithmetic(Mult) -> "`mult` arithmetic"
  | Arithmetic(Divmod) -> "`divmod` arithmetic"
  | Arithmetic(Max) -> "`max` arithmetic"
  | Gate(And) -> "`and` gate"
  | Gate(Not) -> "`not` gate"
  | Gate(Or) -> "`or` gate"
  | Comparator(Geq) -> "`>=` comparator"
  | Comparator(G) -> "`>` comparator"
  | Comparator(Leq) -> "`<=` comparator"
  | Comparator(L) -> "`<` comparator"
  | Comparator(Eq) -> "`=` comparator"
  | Comparator(Neq) -> "`!=` comparator"
  | Intrinsic(Print) -> "`print` intrinsic"
  | Intrinsic(Dup) -> "`dup` intrinsic"
  | Intrinsic(Rot) -> "`rot` intrinsic"
  | Intrinsic(Swap) -> "`swap` intrinsic"
  | Intrinsic(Drop) -> "`drop` intrinsic"
  | Intrinsic(Over) -> "`over` intrinsic";;

let push (elem: 'a) (s: 'a stack) :'a stack = Node(elem, s);;

let pop (s: 'a stack): 'a * 'a stack = match s with
  | Empty -> failwith "Can't pop an empty stack."
  | Node(a,s) -> a, s;;
(*
type sub_prog =
  | Op of prog_token
  | Branch of bool * (prog_token list);;

type prog = sub_prog list;;
*)

type 'a prog_elem =
  | Op of prog_token
  | Branch of ('a * 'a prog)
and 'a prog = ('a prog_elem) list;;


let print_bool b = match b with
  | true -> print_string "true"
  | _ -> print_string "false";;

let compiler_message_error_exp (expected: prog_token) (actual: prog_token) =
  "ERROR: Expected "^(human expected)^" but actually got "^(human actual);;

let ar_plus (s: prog_token stack): prog_token stack =
  let cons_b, s1 = pop s  in
  let cons_a, s2 = pop s1 in
  match cons_b, cons_a with
    | Int(b), Int(a) -> push (Int(a+b)) s2
    | _, Int(_) -> failwith (compiler_message_error_exp (Int(0)) cons_b)
    | Int(_), _ -> failwith (compiler_message_error_exp (Int(0)) cons_a)
    | _, _ -> failwith (compiler_message_error_exp (Int(0)) cons_b);;

let ar_minus (s: prog_token stack): prog_token stack =
  let cons_b, s1 = pop s  in
  let cons_a, s2 = pop s1 in
  match cons_b, cons_a with
    | Int(b), Int(a) -> push (Int(a-b)) s2
    | _, Int(_) -> failwith (compiler_message_error_exp (Int(0)) cons_b)
    | Int(_), _ -> failwith (compiler_message_error_exp (Int(0)) cons_a)
    | _, _ -> failwith (compiler_message_error_exp (Int(0)) cons_b);;

let ar_mult (s: prog_token stack): prog_token stack =
  let cons_b, s1 = pop s  in
  let cons_a, s2 = pop s1 in
  match cons_b, cons_a with
    | Int(b), Int(a) -> push (Int(a*b)) s2
    | _, Int(_) -> failwith (compiler_message_error_exp (Int(0)) cons_b)
    | Int(_), _ -> failwith (compiler_message_error_exp (Int(0)) cons_a)
    | _, _ -> failwith (compiler_message_error_exp (Int(0)) cons_b);;

let ar_divmod (s: prog_token stack): prog_token stack =
  let cons_b, s1 = pop s  in
  let cons_a, s2 = pop s1 in
  match cons_b, cons_a with
    | Int(b), Int(a) -> push (Int(a/b)) (push (Int(a mod b))s2)
    | _, Int(_) -> failwith (compiler_message_error_exp (Int(0)) cons_b)
    | Int(_), _ -> failwith (compiler_message_error_exp (Int(0)) cons_a)
    | _, _ -> failwith (compiler_message_error_exp (Int(0)) cons_b);;

let ar_max (s: prog_token stack): prog_token stack =
  let cons_b, s1 = pop s  in
  let cons_a, s2 = pop s1 in
  match cons_b, cons_a with
    | Int(b), Int(a) -> push (Int(max a b)) s2
    | _, Int(_) -> failwith (compiler_message_error_exp (Int(0)) cons_b)
    | Int(_), _ -> failwith (compiler_message_error_exp (Int(0)) cons_a)
    | _, _ -> failwith (compiler_message_error_exp (Int(0)) cons_b);;

let in_drop (s: prog_token stack): prog_token stack = let _, s1 = pop s in s1;;

let in_print (s: prog_token stack): prog_token stack =
  let elem, s1 = pop s in
  (match elem with 
    | Int(a) -> print_int a
    | Bool(a) -> print_bool a);
    print_endline "";
    s1;;

let in_dup (s: prog_token stack): prog_token stack=
  let a, s1 = pop s in
  let s2 = push a (push a s1) in s2;;

let in_swap (s: prog_token stack): prog_token stack =
  let b, s1 = pop s in
  let a, s2 = pop s1 in
  let s3 = push b s2 in
  let s4 = push a s3 in s4;;

let in_over (s: prog_token stack): prog_token stack =
  let a, s1 = pop s in
  let b, s2 = pop s1 in
  let s3 = push b s2 in
  let s4 = push a s3 in
  let s5 = push b s4 in s5;;

let in_rot (s: prog_token stack): prog_token stack =
  let c, s1 = pop s in
  let b, s2 = pop s1 in
  let a, s3 = pop s2 in
  let s4 = push b s3 in
  let s5 = push c s4 in
  let s6 = push a s5 in s6;;

let co_geq (s: prog_token stack): prog_token stack=
  let cons_b, s1 = pop s  in
  let cons_a, s2 = pop s1 in
  match cons_b, cons_a with
    | Int(b), Int(a) -> push (Bool(a >= b)) s2
    | _, Int(_) -> failwith (compiler_message_error_exp (Int(0)) cons_b)
    | Int(_), _ -> failwith (compiler_message_error_exp (Int(0)) cons_a)
    | _, _ -> failwith (compiler_message_error_exp (Int(0)) cons_b);;

let co_g (s: prog_token stack): prog_token stack=
  let cons_b, s1 = pop s  in
  let cons_a, s2 = pop s1 in
  match cons_b, cons_a with
    | Int(b), Int(a) -> push (Bool(a > b)) s2
    | _, Int(_) -> failwith (compiler_message_error_exp (Int(0)) cons_b)
    | Int(_), _ -> failwith (compiler_message_error_exp (Int(0)) cons_a)
    | _, _ -> failwith (compiler_message_error_exp (Int(0)) cons_b);;

let co_leq (s: prog_token stack): prog_token stack =
  let cons_b, s1 = pop s  in
  let cons_a, s2 = pop s1 in
  match cons_b, cons_a with
    | Int(b), Int(a) -> push (Bool(a <= b)) s2
    | _, Int(_) -> failwith (compiler_message_error_exp (Int(0)) cons_b)
    | Int(_), _ -> failwith (compiler_message_error_exp (Int(0)) cons_a)
    | _, _ -> failwith (compiler_message_error_exp (Int(0)) cons_b);;

let co_l (s: prog_token stack): prog_token stack =
  let cons_b, s1 = pop s  in
  let cons_a, s2 = pop s1 in
  match cons_b, cons_a with
    | Int(b), Int(a) -> push (Bool(a < b)) s2
    | _, Int(_) -> failwith (compiler_message_error_exp (Int(0)) cons_b)
    | Int(_), _ -> failwith (compiler_message_error_exp (Int(0)) cons_a)
    | _, _ -> failwith (compiler_message_error_exp (Int(0)) cons_b);;

let co_eq (s: prog_token stack): prog_token stack =
  let cons_b, s1 = pop s  in
  let cons_a, s2 = pop s1 in
  match cons_b, cons_a with
    | Int(b), Int(a) -> push (Bool(a = b)) s2
    | Bool(b), Bool(a) -> push (Bool(a = b)) s2
    | _, _ -> failwith (compiler_message_error_exp cons_a cons_b);;

let co_neq (s: prog_token stack): prog_token stack =
  let cons_b, s1 = pop s  in
  let cons_a, s2 = pop s1 in
  match cons_b, cons_a with
    | Int(b), Int(a) -> push (Bool(a <> b)) s2
    | Bool(b), Bool(a) -> push (Bool(a <> b)) s2
    | _, _ -> failwith (compiler_message_error_exp cons_a cons_b);;

let parse_prog (p: 'a prog): prog_token stack = 
  let rec aux p acc = match p with
    | [] -> acc
    | Op(Arithmetic(Plus))::q -> aux q (ar_plus acc)
    | Op(Arithmetic(Minus))::q -> aux q (ar_minus acc)
    | Op(Arithmetic(Mult))::q -> aux q (ar_mult acc)
    | Op(Arithmetic(Divmod))::q -> aux q (ar_divmod acc)
    | Op(Arithmetic(Max))::q -> aux q (ar_max acc)
    | Op(Intrinsic(Drop))::q -> aux q (in_drop acc)
    | Op(Intrinsic(Print))::q -> aux q (in_print acc)
    | Op(Intrinsic(Dup))::q -> aux q (in_dup acc)
    | Op(Intrinsic(Swap))::q -> aux q (in_swap acc)
    | Op(Intrinsic(Over))::q -> aux q (in_over acc)
    | Op(Intrinsic(Rot))::q -> aux q (in_rot acc)
    | Op(Comparator(Geq))::q -> aux q (co_geq acc)
    | Op(Comparator(G))::q -> aux q (co_g acc)
    | Op(Comparator(Leq))::q -> aux q (co_leq acc)
    | Op(Comparator(L))::q -> aux q (co_l acc)
    | Op(Comparator(Eq))::q -> aux q (co_eq acc)
    | Op(Comparator(Neq))::q -> aux q (co_neq acc)
    | Op(Gate(Or))::q -> (
      let Bool(b), s1 = pop acc in
      let Bool(a), s2 = pop s1 in
      aux q (push (Bool(a||b)) s2))
    | Op(Gate(And))::q -> (
      let Bool(b), s1 = pop acc in
      let Bool(a), s2 = pop s1 in
      aux q (push (Bool(a&&b)) s2))
    | Op(Gate(Not))::q -> (
      let Bool(a), s1 = pop acc in
      aux q (push (Bool(not a)) s1))
    | Op(e)::q -> aux q (push e acc) in
  aux p Empty;;

let splice_line (s: string): string list =
  let l = String.split_on_char ' ' (String.trim s) in
  match l with
    | [""] -> []
    | l -> l;;

let get_prog_from_file (filename: string): string list =
  let res = ref [] in 
  let ic = open_in filename in
  try while true; do
    let line = input_line ic in
    res := !res@(splice_line line); done;!res
  with End_of_file -> close_in ic;
  !res;;



let get_tok_l (l: string list): prog_token list =
  let rec aux l = match l with
    | [] -> []
    | "+"::q -> Arithmetic(Plus)::(aux q )
    | "-"::q -> Arithmetic(Minus)::(aux q )
    | "*"::q -> Arithmetic(Mult)::(aux q )
    | "divmod"::q -> Arithmetic(Divmod)::(aux q )
    | "max"::q -> Arithmetic(Max)::(aux q )
    | "print"::q -> Intrinsic(Print)::(aux q )
    | "dup"::q -> Intrinsic(Dup)::(aux q )
    | "swap"::q -> Intrinsic(Swap)::(aux q )
    | "drop"::q -> Intrinsic(Drop)::(aux q )
    | "rot"::q -> Intrinsic(Rot)::(aux q )
    | "over"::q -> Intrinsic(Over)::(aux q )
    | "true"::q -> Bool(true)::(aux q )
    | "false"::q -> Bool(false)::(aux q )
    | "<="::q -> Comparator(Leq)::(aux q )
    | "<"::q -> Comparator(L)::(aux q )
    | ">="::q -> Comparator(Geq)::(aux q )
    | ">"::q -> Comparator(G)::(aux q )
    | "="::q -> Comparator(Eq)::(aux q )
    | "!="::q -> Comparator(Neq)::(aux q )
    | "!!"::q -> Gate(Not)::(aux q )
    | "&&"::q -> Gate(And)::(aux q )
    | "||"::q -> Gate(Or)::(aux q )
    | "if"::q -> Keyword(If)::(aux q)
    | "else"::q -> Keyword(Else)::(aux q)
    | "end"::q -> Keyword(End)::(aux q)
    | e::q -> Int(int_of_string e)::(aux q ) in
  aux l;;
(*
let run_programme p = let _ = parse_prog p in ();;

let run_programme_from_file (filename: string) = 
  let programme = get_prog_from_file filename in
  let p =  get_prog_from_l_o_t programme in
  parse_prog p;;*)
(*
run_programme_from_file "test_prog.txt";;
get_prog_from_file "test_prog.txt";;
*)



let string_l = get_prog_from_file "test_prog.txt"
let tok_l = get_tok_l string_l;;