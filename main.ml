type intrinsic =
  | Print
  | Dup
  | Swap
  | Rot
  | Drop
  | Over

type keyword =
  | If
  | Else
  | While
  | End
  | Do
  | Proc
  | ProcEnd


type arithmetic =
  | Plus
  | Mult
  | Minus
  | Divmod
  | Max

type comparator =
  | Geq
  | Leq
  | G
  | L
  | Eq
  | Neq

type gate =
  | And
  | Or
  | Not

type prog_token =
  | NoOp
  | Intrinsic of intrinsic
  | Arithmetic of arithmetic
  | Comparator of comparator
  | Gate of gate
  | Keyword of keyword
  | Int of int
  | Bool of bool
  | Str of string
  | Identifier of string
;;

type program =
  | Exp of prog_token
  | Sub of sub_type * subprogram * subprogram * bool
and subprogram = program list
and sub_type =
  | Cond
  | Loop of subprogram
  | Main;;

type 'a stack =
  | Empty
  | Node of 'a * 'a stack;;

let push (elem : 'a) (s : 'a stack) : 'a stack = Node (elem, s);;

let rec get_stack_len (s: 'a stack) = match s with
  | Node(_, a) -> 1+get_stack_len a
  | Empty -> 0;;



let pop (s : 'a stack) : 'a * 'a stack =
  match s with
  | Empty -> failwith "Can't pop an empty stack."
  | Node (a, s) -> a, s
;;

let human (t : prog_token) : string =
  match t with
  | Int _ -> "`integer`"
  | Bool _ -> "`bool` type"
  | Arithmetic Plus -> "`plus` arithmetic"
  | Arithmetic Minus -> "`minus` arithmetic"
  | Arithmetic Mult -> "`mult` arithmetic"
  | Arithmetic Divmod -> "`divmod` arithmetic"
  | Arithmetic Max -> "`max` arithmetic"
  | Gate And -> "`and` gate"
  | Gate Not -> "`not` gate"
  | Gate Or -> "`or` gate"
  | Comparator Geq -> "`>=` comparator"
  | Comparator G -> "`>` comparator"
  | Comparator Leq -> "`<=` comparator"
  | Comparator L -> "`<` comparator"
  | Comparator Eq -> "`=` comparator"
  | Comparator Neq -> "`!=` comparator"
  | Intrinsic Print -> "`print` intrinsic"
  | Intrinsic Dup -> "`dup` intrinsic"
  | Intrinsic Rot -> "`rot` intrinsic"
  | Intrinsic Swap -> "`swap` intrinsic"
  | Intrinsic Drop -> "`drop` intrinsic"
  | Intrinsic Over -> "`over` intrinsic"
  |_ -> failwith "TODO: not implemented yet."
;;

let print_bool b =
  match b with
  | true -> print_string "true"
  | _ -> print_string "false"
;;

let compiler_message_error_exp (expected : prog_token) (actual : prog_token) =
  "ERROR: Expected " ^ human expected ^ " but actually got " ^ human actual
;;

let ar_plus (s : prog_token stack) : prog_token stack =
  let cons_b, s1 = pop s in
  let cons_a, s2 = pop s1 in
  match cons_b, cons_a with
  | Int b, Int a -> push (Int (a + b)) s2
  | _, Int _ -> failwith (compiler_message_error_exp (Int 0) cons_b)
  | Int _, _ -> failwith (compiler_message_error_exp (Int 0) cons_a)
  | _, _ -> failwith (compiler_message_error_exp (Int 0) cons_b)
;;

let ar_minus (s : prog_token stack) : prog_token stack =
  let cons_b, s1 = pop s in
  let cons_a, s2 = pop s1 in
  match cons_b, cons_a with
  | Int b, Int a -> push (Int (a - b)) s2
  | _, Int _ -> failwith (compiler_message_error_exp (Int 0) cons_b)
  | Int _, _ -> failwith (compiler_message_error_exp (Int 0) cons_a)
  | _, _ -> failwith (compiler_message_error_exp (Int 0) cons_b)
;;

let ar_mult (s : prog_token stack) : prog_token stack =
  let cons_b, s1 = pop s in
  let cons_a, s2 = pop s1 in
  match cons_b, cons_a with
  | Int b, Int a -> push (Int (a * b)) s2
  | _, Int _ -> failwith (compiler_message_error_exp (Int 0) cons_b)
  | Int _, _ -> failwith (compiler_message_error_exp (Int 0) cons_a)
  | _, _ -> failwith (compiler_message_error_exp (Int 0) cons_b)
;;

let ar_divmod (s : prog_token stack) : prog_token stack =
  let cons_b, s1 = pop s in
  let cons_a, s2 = pop s1 in
  match cons_b, cons_a with
  | Int b, Int a -> push (Int (a / b)) (push (Int (a mod b)) s2)
  | _, Int _ -> failwith (compiler_message_error_exp (Int 0) cons_b)
  | Int _, _ -> failwith (compiler_message_error_exp (Int 0) cons_a)
  | _, _ -> failwith (compiler_message_error_exp (Int 0) cons_b)
;;

let ar_max (s : prog_token stack) : prog_token stack =
  let cons_b, s1 = pop s in
  let cons_a, s2 = pop s1 in
  match cons_b, cons_a with
  | Int b, Int a -> push (Int (max a b)) s2
  | _, Int _ -> failwith (compiler_message_error_exp (Int 0) cons_b)
  | Int _, _ -> failwith (compiler_message_error_exp (Int 0) cons_a)
  | _, _ -> failwith (compiler_message_error_exp (Int 0) cons_b)
;;

let in_drop (s : prog_token stack) : prog_token stack =
  let _, s1 = pop s in
  s1
;;

let in_print (s : prog_token stack) : prog_token stack =
  let elem, s1 = pop s in
  (match elem with
  | Int a -> print_int a
  | Bool a -> print_bool a
  | Str a -> print_string a
  | _ -> failwith "TODO: only supported types are `int` and `bool` types.");
  s1
;;

let in_dup (s : prog_token stack) : prog_token stack =
  let a, s1 = pop s in
  let s2 = push a (push a s1) in
  s2
;;

let in_swap (s : prog_token stack) : prog_token stack =
  let b, s1 = pop s in
  let a, s2 = pop s1 in
  let s3 = push b s2 in
  let s4 = push a s3 in
  s4
;;

let in_over (s : prog_token stack) : prog_token stack =
  let a, s1 = pop s in
  let b, s2 = pop s1 in
  let s3 = push b s2 in
  let s4 = push a s3 in
  let s5 = push b s4 in
  s5
;;

let in_rot (s : prog_token stack) : prog_token stack =
  let c, s1 = pop s in
  let b, s2 = pop s1 in
  let a, s3 = pop s2 in
  let s4 = push b s3 in
  let s5 = push c s4 in
  let s6 = push a s5 in
  s6
;;

let co_geq (s : prog_token stack) : prog_token stack =
  let cons_b, s1 = pop s in
  let cons_a, s2 = pop s1 in
  match cons_b, cons_a with
  | Int b, Int a -> push (Bool (a >= b)) s2
  | _, Int _ -> failwith (compiler_message_error_exp (Int 0) cons_b)
  | Int _, _ -> failwith (compiler_message_error_exp (Int 0) cons_a)
  | _, _ -> failwith (compiler_message_error_exp (Int 0) cons_b)
;;

let co_g (s : prog_token stack) : prog_token stack =
  let cons_b, s1 = pop s in
  let cons_a, s2 = pop s1 in
  match cons_b, cons_a with
  | Int b, Int a -> push (Bool (a > b)) s2
  | _, Int _ -> failwith (compiler_message_error_exp (Int 0) cons_b)
  | Int _, _ -> failwith (compiler_message_error_exp (Int 0) cons_a)
  | _, _ -> failwith (compiler_message_error_exp (Int 0) cons_b)
;;

let co_leq (s : prog_token stack) : prog_token stack =
  let cons_b, s1 = pop s in
  let cons_a, s2 = pop s1 in
  match cons_b, cons_a with
  | Int b, Int a -> push (Bool (a <= b)) s2
  | _, Int _ -> failwith (compiler_message_error_exp (Int 0) cons_b)
  | Int _, _ -> failwith (compiler_message_error_exp (Int 0) cons_a)
  | _, _ -> failwith (compiler_message_error_exp (Int 0) cons_b)
;;

let co_l (s : prog_token stack) : prog_token stack =
  let cons_b, s1 = pop s in
  let cons_a, s2 = pop s1 in
  match cons_b, cons_a with
  | Int b, Int a -> push (Bool (a < b)) s2
  | _, Int _ -> failwith (compiler_message_error_exp (Int 0) cons_b)
  | Int _, _ -> failwith (compiler_message_error_exp (Int 0) cons_a)
  | _, _ -> failwith (compiler_message_error_exp (Int 0) cons_b)
;;

let co_eq (s : prog_token stack) : prog_token stack =
  let cons_b, s1 = pop s in
  let cons_a, s2 = pop s1 in
  match cons_b, cons_a with
  | Int b, Int a -> push (Bool (a = b)) s2
  | Bool b, Bool a -> push (Bool (a = b)) s2
  | _, _ -> failwith (compiler_message_error_exp cons_a cons_b)
;;

let co_neq (s : prog_token stack) : prog_token stack =
  let cons_b, s1 = pop s in
  let cons_a, s2 = pop s1 in
  match cons_b, cons_a with
  | Int b, Int a -> push (Bool (a <> b)) s2
  | Bool b, Bool a -> push (Bool (a <> b)) s2
  | _, _ -> failwith (compiler_message_error_exp cons_a cons_b)
;;

let ga_or (s : prog_token stack) : prog_token stack =
  let b, s1 = pop s in
  let a, s2 = pop s1 in
  match a, b with
  | Bool a', Bool b' -> push (Bool (a' || b')) s2
  | _ -> failwith "expected bool but found int"
;;

let ga_and (s : prog_token stack) : prog_token stack =
  let b, s1 = pop s in
  let a, s2 = pop s1 in
  match a, b with
  | Bool a', Bool b' -> push (Bool (a' && b')) s2
  | _ -> failwith "expected bool but found int"
;;

let ga_not (s : prog_token stack) : prog_token stack =
  let a, s1 = pop s in
  match a with
     |Bool b -> push (Bool (not b)) s1
     | _ -> failwith "Expected `bool` type for `not` gate."
;;

let is_int (s: string): bool =
  let numerals = ['-'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; '0'] in
  if String.length s = 0 then false
  else (
    let c = s.[0] in
    List.exists (fun a -> a = c) numerals;
  );;

let is_str (s: string): bool =
    if String.length s = 0 then false
    else '\"' = s.[0];;


let get_tok_l (l : string list) : prog_token list =
  let rec aux l =
    match l with
    | [] -> []
    | "+" :: q -> Arithmetic Plus :: aux q
    | "-" :: q -> Arithmetic Minus :: aux q
    | "*" :: q -> Arithmetic Mult :: aux q
    | "divmod" :: q -> Arithmetic Divmod :: aux q
    | "max" :: q -> Arithmetic Max :: aux q
    | "print" :: q -> Intrinsic Print :: aux q
    | "dup" :: q -> Intrinsic Dup :: aux q
    | "swap" :: q -> Intrinsic Swap :: aux q
    | "drop" :: q -> Intrinsic Drop :: aux q
    | "rot" :: q -> Intrinsic Rot :: aux q
    | "over" :: q -> Intrinsic Over :: aux q
    | "true" :: q -> Bool true :: aux q
    | "false" :: q -> Bool false :: aux q
    | "<=" :: q -> Comparator Leq :: aux q
    | "<" :: q -> Comparator L :: aux q
    | ">=" :: q -> Comparator Geq :: aux q
    | ">" :: q -> Comparator G :: aux q
    | "=" :: q -> Comparator Eq :: aux q
    | "!=" :: q -> Comparator Neq :: aux q
    | "!!" :: q -> Gate Not :: aux q
    | "&&" :: q -> Gate And :: aux q
    | "||" :: q -> Gate Or :: aux q
    | "if" :: q -> Keyword If :: aux q
    | "else" :: q -> Keyword Else :: aux q
    | "while" :: q -> Keyword While :: aux q
    | "do" :: q -> Keyword Do :: aux q
    | "end" :: q -> Keyword End :: aux q
    | "proc" :: q -> Keyword Proc :: aux q
    | "procend" :: q -> Keyword ProcEnd :: aux q 
    | e :: q when is_int e -> Int (int_of_string e) :: aux q
    | e :: q when is_str e -> let n = String.length e in Str (String.sub e 1 (n-2)) :: aux q
    | e :: q -> Identifier e :: aux q
  in
  aux l
;;

let get_proc_tokens tok_l =
  let rec aux l acc = match l with
    | Keyword ProcEnd :: q -> acc, q
    | tok :: q -> aux q (acc@[tok])
    | [] -> failwith "Procedures should always be closed with `procend` keyword." in
  aux tok_l [];;

let get_proc_id name name_array =
  let n = Array.length name_array in
  let res = ref (-1) in
  for i = 0 to n-1 do
    if name_array.(i) = name then res := i;
  done;
  match !res with
    | -1 -> failwith ("`"^name^"`"^" procedure used before declaration.")
    | _ -> !res;;


let create_program_tree (tok_l : prog_token list) =
  let max_proc_count = 100 in
  let proc_array: subprogram array = Array.make max_proc_count [] in
  let name_array: string array = Array.make max_proc_count "" in
  let proc_count = ref 0 in
  let rec aux (tok_l : prog_token list) (s : program stack) (proc_flag: bool): program =
    match tok_l with
    | [] ->
      let p, s1 = pop s in p
    | Keyword If :: q -> aux q (push (Sub (Cond, [], [], false)) s) proc_flag
    | Keyword While :: q -> aux q (push (Sub (Main, [], [], false)) s) proc_flag
    | Keyword Do :: q ->
      let condition, s1 = pop s in
      let new_head = Sub (Loop ([condition]), [], [], false) in
      aux q (push new_head s1) proc_flag
    | Keyword Else :: q ->
      let p, s1 = pop s in
      let sub =
        match p with
        | Sub (Cond, if_branch, else_branch, false) ->
          Sub (Cond, if_branch, else_branch, true)
        | _ -> failwith "Syntax Error"
      in
      aux q (push sub s1) proc_flag
    | Keyword End :: q ->
      let p, s1 = pop s in
      let () =
        match p with
        | Sub ((Cond | Loop _), _, _, _) -> ()
        | _ -> failwith "`end` keyword can only close `if-else`  and `while` blocks."
      in
      let new_head, s2 = pop s1 in
      let h =
        match new_head with
        | Sub (a, b, c, false) -> Sub (a, b @ [ p ], c, false)
        | Sub (a, b, c, true) -> Sub (a, b, c @ [ p ], true)
        | Exp tok -> Sub (Main, [ Exp tok; p ], [], false)
      in
      aux q (push h s2) proc_flag
    | Keyword Proc :: Identifier proc_name :: q ->
      let () = match proc_flag with
        | true -> failwith "Cannot create a procedure inside of a procedure."
        | _ -> () in
      name_array.(!proc_count) <- proc_name;
      let proc_tok_l, rest = get_proc_tokens q in
      let proc_content = aux proc_tok_l (Node (Sub (Main, [], [], false), Empty)) true in
      let content = (match proc_content with
        | Sub (_, l, _, _) -> l
        | _ -> failwith "Syntax error") in
      proc_array.(!proc_count) <- content;
      incr proc_count;
      aux rest s false;
    | Keyword ProcEnd :: q -> let p, s1 = pop s in p
    | token :: q ->
      let p, s1 = pop s in
      let sub =
        match p with
        | Sub (t, if_branch, else_branch, false) ->
          Sub (t, if_branch @ [ Exp token ], else_branch, false)
        | Sub (Cond, if_branch, else_branch, true) ->
          Sub (Cond, if_branch, else_branch @ [ Exp token ], true)
        | _ -> failwith "Syntax Error"
      in
      aux q (push sub s1) proc_flag
  in
  ((aux tok_l (Node (Sub (Main, [], [], false), Empty)) false), proc_array, name_array)
;;

let eval_program (p : program) proc_array name_array =
  let rec aux p  acc =
    match p with
    | [] -> acc
    | Exp (Arithmetic Plus) :: q -> aux q (ar_plus acc)
    | Exp (Arithmetic Minus) :: q -> aux q (ar_minus acc)
    | Exp (Arithmetic Mult) :: q -> aux q (ar_mult acc)
    | Exp (Arithmetic Divmod) :: q -> aux q (ar_divmod acc)
    | Exp (Arithmetic Max) :: q -> aux q (ar_max acc)
    | Exp (Intrinsic Drop) :: q -> aux q (in_drop acc)
    | Exp (Intrinsic Print) :: q -> aux q (in_print acc)
    | Exp (Intrinsic Dup) :: q -> aux q (in_dup acc)
    | Exp (Intrinsic Swap) :: q -> aux q (in_swap acc)
    | Exp (Intrinsic Over) :: q -> aux q (in_over acc)
    | Exp (Intrinsic Rot) :: q -> aux q (in_rot acc)
    | Exp (Comparator Geq) :: q -> aux q (co_geq acc)
    | Exp (Comparator G) :: q -> aux q (co_g acc)
    | Exp (Comparator Leq) :: q -> aux q (co_leq acc)
    | Exp (Comparator L) :: q -> aux q (co_l acc)
    | Exp (Comparator Eq) :: q -> aux q (co_eq acc)
    | Exp (Comparator Neq) :: q -> aux q (co_neq acc)
    | Exp (Gate Or) :: q -> aux q (ga_or acc)
    | Exp (Gate And) :: q -> aux q (ga_and acc)
    | Exp (Gate Not) :: q -> aux q (ga_not acc)
    | Exp (Identifier proc_name) :: q ->
      let proc_id = get_proc_id proc_name name_array in
      let proc_prog = proc_array.(proc_id) in
      aux (proc_prog@q) acc; 
    | Exp e :: q -> aux q (push e acc)
    | Sub (Main, a, _, _) :: q -> aux q (aux a acc)
    | Sub (Cond, if_branch, else_branch, _) :: q ->
      let tok, s1 = pop acc in
      let new_stack =
        match tok with
        | Bool true -> aux if_branch s1
        | Bool false -> aux else_branch s1
        | _ ->
          failwith "Expected `bool` type as head of the stack before `if-else`block."
        in
        aux q new_stack
    | Sub (Loop condition, prog, _, _)::q ->
        let s1 = aux condition acc in
        let tok, s2 = pop s1 in
        match tok with
          | Bool true -> let new_stack = aux prog s2 in aux p new_stack
          | Bool false -> aux q s2
          | _ -> failwith "Expected `bool` type as head of the stack before `while` block."
  in
  match p with
  | Sub (Main, a, _, _) -> aux a Empty
  | _ -> failwith "Syntax error."
;;

let splice_line (s : string) : string list =
  let l = String.split_on_char ' ' (String.trim s) in
  match l with
  | [ "" ] -> []
  | l -> l
;;

let rec discard_comments li = match li with
  | [] -> []
  | '/'::'/'::_ -> []
  | e::q -> e::(discard_comments q);;

let get_list_of_chars (s: string): char list =
  let n = String.length s in
  let res = ref [] in
  for i = 0 to n-1 do
    res := !res@[s.[i]];
  done;
  !res;;
  

let get_char_list_from_file (filename: string): char list =
  let res = ref [] in
  let ic = open_in filename in
  let () = try
      while true do
        let line = input_line ic in
        let li = discard_comments (get_list_of_chars line) in
        res := !res@('\n'::li);
      done;
    with
      | End_of_file -> close_in ic; in
    !res;;


let get_prog_from_file (filename : string) : string list =
  let res = ref [] in
  let ic = open_in filename in
  try
    while true do
      let line = input_line ic in
      res := !res @ splice_line line
    done;
    !res
  with
  | End_of_file ->
    close_in ic;
    !res
;;


let get_string_of_char = String.make 1;;

let get_list_of_unprocessed_tokens l = 
  let rec aux l li_acc st_acc st_flag = match l with
    | '\"'::q when st_flag = false -> aux q li_acc (get_string_of_char '\"') true
    | '\"'::q -> aux q (li_acc@[st_acc^(get_string_of_char '\"')]) "" false
    | '\n'::q when st_flag = false -> aux q (li_acc@[st_acc]) "" false
    | ' ' ::q when st_flag = false -> aux q (li_acc@[st_acc]) "" false
    | e::q -> aux q li_acc (st_acc^(get_string_of_char e)) st_flag
    | [] -> li_acc@[st_acc] in
  let rec aux2 l = match l with
    | ""::q -> aux2 q
    | e::q -> e::aux2 q
    | [] -> [] in
  let l1 = aux l [] "" false in
  aux2 l1;;

let get_string_list_from_file filename =
  let l = get_char_list_from_file filename in
  get_list_of_unprocessed_tokens l;;

let get_basic_main_prog () = 
  Sub (Main, [Exp (Identifier "main")], [], false);;

let interpret_file (filename: string) =
  let string_l = get_string_list_from_file filename in
  let tok_l = get_tok_l string_l in
  let _, proc_array, name_array = create_program_tree tok_l in
  let _ = eval_program (get_basic_main_prog ()) proc_array name_array in ();;

interpret_file "test_prog.plth";;

