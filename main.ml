type intrinsic =
  | Print
  | Dup
  | Swap
  | Rot
  | Drop
  | Over
  | Length

type declarator =
  | Proc
  | ProcEnd
  | Var

type debugger = DebugDump

type keyword =
  | If
  | Else
  | While
  | End
  | Do
  | Include
  | Dot
  | Transfer
  | Dump
  | Purge
  | In

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
  | Declarator of declarator
  | Int of int
  | Bool of bool
  | Str of string
  | Identifier of string
  | Deb of debugger

type program =
  | Exp of prog_token
  | Sub of sub_type * subprogram * subprogram * bool

and subprogram = program list

and sub_type =
  | Cond
  | Loop of subprogram
  | Main

type 'a stack =
  | Empty
  | Node of 'a * 'a stack

let push (elem : 'a) (s : 'a stack) : 'a stack = Node (elem, s)

let rec get_stack_len (s : 'a stack) =
  match s with
  | Node (_, a) -> 1 + get_stack_len a
  | Empty -> 0
;;

let rec append_stacks s1 s2 =
  match s1 with
  | Empty -> s2
  | Node (a, b) -> Node (a, append_stacks b s2)
;;

let pop (s : 'a stack) : 'a * 'a stack =
  match s with
  | Empty -> failwith "Can't pop an empty stack."
  | Node (a, s) -> a, s
;;

let human (t : prog_token) : string =
  match t with
  | Int _ -> "`integer` type"
  | Bool _ -> "`bool` type"
  | Str _ -> "`string` type"
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
  | _ -> ""
;;

let print_bool b =
  match b with
  | true -> print_string "true"
  | _ -> print_string "false"
;;

let human_values t =
  match t with
  | Int a -> print_int a
  | Bool a -> print_bool a
  | Str a -> print_string ("\"" ^ a ^ "\"")
  | _ -> ()
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
  | Str st2, Str st1 -> push (Str (st1 ^ st2)) s2
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

let rec dump_stack (s : prog_token stack) =
  match s with
  | Empty -> ()
  | Node (Str a, b) ->
    print_string (human (Str a));
    print_string "  --> ";
    human_values (Str a);
    print_endline "";
    dump_stack b
  | Node (a, b) ->
    print_string (human a);
    print_string " --> ";
    human_values a;
    print_endline "";
    dump_stack b
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
  push a (push a s1)
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
  | Str str1, Str str2 -> push (Bool (str1 = str2)) s2
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
  | Bool b -> push (Bool (not b)) s1
  | _ -> failwith "Expected `bool` type for `not` gate."
;;

let is_int (s : string) : bool =
  let numerals = [ '-'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; '0' ] in
  if String.length s = 0
  then false
  else (
    let c = s.[0] in
    List.exists (fun a -> a = c) numerals)
;;

let is_str (s : string) : bool = if String.length s = 0 then false else '\"' = s.[0]

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
    | "include" :: q -> Keyword Include :: aux q
    | "." :: q -> Keyword Dot :: aux q
    | "dump" :: q -> Keyword Dump :: aux q
    | "purge" :: q -> Keyword Purge :: aux q
    | "transfer" :: q -> Keyword Transfer :: aux q
    | "in" :: q -> Keyword In :: aux q
    | "proc" :: q -> Declarator Proc :: aux q
    | "procend" :: q -> Declarator ProcEnd :: aux q
    | "var" :: q -> Declarator Var :: aux q
    | "debugdump" :: q -> Deb DebugDump :: aux q
    | "length" :: q -> Intrinsic Length :: aux q
    | e :: q when is_int e -> Int (int_of_string e) :: aux q
    | e :: q when is_str e ->
      let n = String.length e in
      Str (String.sub e 1 (n - 2)) :: aux q
    | e :: q -> Identifier e :: aux q
  in
  aux l
;;

let get_string_of_char = String.make 1

let get_list_of_unprocessed_tokens l =
  let rec aux l li_acc st_acc st_flag =
    match l with
    | '\"' :: q when st_flag = false -> aux q li_acc (get_string_of_char '\"') true
    | '\"' :: q -> aux q (li_acc @ [ st_acc ^ get_string_of_char '\"' ]) "" false
    | '\n' :: q when st_flag = false -> aux q (li_acc @ [ st_acc ]) "" false
    | ' ' :: q when st_flag = false -> aux q (li_acc @ [ st_acc ]) "" false
    | e :: q -> aux q li_acc (st_acc ^ get_string_of_char e) st_flag
    | [] -> li_acc @ [ st_acc ]
  in
  let rec aux2 l =
    match l with
    | "" :: q -> aux2 q
    | e :: q -> e :: aux2 q
    | [] -> []
  in
  let l1 = aux l [] "" false in
  aux2 l1
;;

let rec discard_comments li =
  match li with
  | [] -> []
  | '/' :: '/' :: _ -> []
  | e :: q -> e :: discard_comments q
;;

let get_list_of_chars (s : string) : char list =
  let n = String.length s in
  let res = ref [] in
  for i = 0 to n - 1 do
    res := !res @ [ s.[i] ]
  done;
  !res
;;

let get_char_list_from_file (filename : string) : char list =
  let res = ref [] in
  let ic = open_in filename in
  let () =
    try
      while true do
        let line = input_line ic in
        let li = discard_comments (get_list_of_chars line) in
        res := !res @ ('\n' :: li)
      done
    with
    | End_of_file -> close_in ic
  in
  !res
;;

let get_string_list_from_file filename =
  let l = get_char_list_from_file filename in
  get_list_of_unprocessed_tokens l
;;

let get_proc_tokens tok_l =
  let rec aux l acc =
    match l with
    | Declarator ProcEnd :: q -> acc, q
    | tok :: q -> aux q (acc @ [ tok ])
    | [] -> failwith "Procedures should always be closed with `procend` keyword."
  in
  aux tok_l []
;;

let get_proc_id name name_array =
  let n = Array.length name_array in
  let res = ref (-1) in
  for i = 0 to n - 1 do
    if name_array.(i) = name then res := i
  done;
  match !res with
  | -1 -> failwith ("`" ^ name ^ "`" ^ " procedure used before declaration.")
  | _ -> !res
;;

let get_proc_count name_arr =
  let res = ref 0 in
  let flag = ref true in
  while !flag do
    if name_arr.(!res) = "" then flag := false else incr res
  done;
  !res
;;

let get_global_path (filename : string) =
  let l = get_list_of_chars filename in
  let last = ref (-1) in
  let rec aux l acc =
    match l with
    | [] -> ()
    | '/' :: q ->
      last := acc;
      aux q (acc + 1)
    | e :: q -> aux q (acc + 1)
  in
  aux l 0;
  match !last with
  | -1 -> filename
  | _ -> String.sub filename 0 (!last + 1)
;;

let update_proc_with_include proc_arr name_arr inc_proc_arr inc_name_arr =
  let proc_count = get_proc_count name_arr in
  let inc_proc_count = get_proc_count inc_name_arr in
  for i = 0 to inc_proc_count do
    proc_arr.(proc_count + i) <- inc_proc_arr.(i);
    name_arr.(proc_count + i) <- inc_name_arr.(i)
  done
;;

let rec create_program_tree (tok_l : prog_token list) filename =
  let max_proc_count = 100 in
  let proc_array : subprogram array = Array.make max_proc_count [] in
  let name_array : string array = Array.make max_proc_count "" in
  let proc_count = ref 0 in
  let rec aux (tok_l : prog_token list) (s : program stack) (proc_flag : bool) : program =
    match tok_l with
    | [] ->
      let p, s1 = pop s in
      p
    | Keyword If :: q -> aux q (push (Sub (Cond, [], [], false)) s) proc_flag
    | Keyword While :: q -> aux q (push (Sub (Main, [], [], false)) s) proc_flag
    | Keyword Do :: q ->
      let condition, s1 = pop s in
      let new_head = Sub (Loop [ condition ], [], [], false) in
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
    | Declarator Var :: Identifier stack_name :: q ->
      let p, s1 = pop s in
      let sub =
        match p with
        | Sub (t, if_branch, else_branch, false) ->
          Sub
            ( t
            , if_branch @ [ Exp (Declarator Var); Exp (Identifier stack_name) ]
            , else_branch
            , false )
        | Sub (Cond, if_branch, else_branch, true) ->
          Sub
            ( Cond
            , if_branch
            , else_branch @ [ Exp (Declarator Var); Exp (Identifier stack_name) ]
            , true )
        | _ -> failwith "Syntax Error"
      in
      aux q (push sub s1) proc_flag
    | Keyword In :: Identifier stack_name :: q ->
      let p, s1 = pop s in
      let sub =
        match p with
        | Sub (t, if_branch, else_branch, false) ->
          Sub
            ( t
            , if_branch @ [ Exp (Keyword In); Exp (Identifier stack_name) ]
            , else_branch
            , false )
        | Sub (Cond, if_branch, else_branch, true) ->
          Sub
            ( Cond
            , if_branch
            , else_branch @ [ Exp (Keyword In); Exp (Identifier stack_name) ]
            , true )
        | _ -> failwith "Syntax Error"
      in
      aux q (push sub s1) proc_flag
    | Declarator Proc :: Identifier proc_name :: q ->
      let () =
        match proc_flag with
        | true -> failwith "Cannot create a procedure inside of a procedure."
        | _ -> ()
      in
      name_array.(!proc_count) <- proc_name;
      let proc_tok_l, rest = get_proc_tokens q in
      let proc_content = aux proc_tok_l (Node (Sub (Main, [], [], false), Empty)) true in
      let content =
        match proc_content with
        | Sub (_, l, _, _) -> l
        | _ -> failwith "Syntax error"
      in
      proc_array.(!proc_count) <- content;
      incr proc_count;
      aux rest s false
    | Declarator ProcEnd :: q ->
      let p, s1 = pop s in
      p
    | Keyword Include :: Str include_filename :: q ->
      let global_include_filename = get_global_path filename ^ include_filename in
      let include_file_tree, inc_procs, inc_names =
        get_prog_tree_from_file global_include_filename filename
      in
      update_proc_with_include proc_array name_array inc_procs inc_names;
      proc_count := get_proc_count inc_names;
      let p, s1 = pop s in
      let to_push =
        match p with
        | Sub (a, b, c, d) -> Sub (a, b @ [ include_file_tree ], c, d)
        | Exp a -> Sub (Main, [ Exp a; include_file_tree ], [], false)
      in
      aux q (push to_push s1) proc_flag
    | Keyword Include :: _ -> failwith "Expected file path after `include` keyword."
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
  aux tok_l (Node (Sub (Main, [], [], false), Empty)) false, proc_array, name_array

and get_prog_tree_from_file (filename : string) f2 =
  let string_l = get_string_list_from_file filename in
  let tok_l = get_tok_l string_l in
  create_program_tree tok_l f2
;;

let is_valid_name name arr =
  let res = ref true in
  for i = 0 to Array.length arr - 1 do
    if arr.(i) = name then res := false
  done;
  !res
;;

let get_name_index name arr =
  let res = ref (-1) in
  for i = 0 to (Array.length arr) - 1 do
    if arr.(i) = name then res := i;
  done;
  match !res with
    | -1 -> failwith ("Unknown stack name `"^name^"`.")
    | a -> a;;

let eval_program (p : program) proc_array name_array =
  let stack_capacity = 100 in
  let stacks = Array.make stack_capacity Empty in
  let names = Array.make stack_capacity "" in
  names.(0) <- "default";
  let stack_count = ref 1 in
  let current_stack = ref 0 in
  let rec aux p =
    match p with
    | [] -> stacks.(0)
    | Exp (Arithmetic Plus) :: q ->
      stacks.(!current_stack) <- ar_plus stacks.(!current_stack);
      aux q
    | Exp (Arithmetic Minus) :: q ->
      stacks.(!current_stack) <- ar_minus stacks.(!current_stack);
      aux q
    | Exp (Arithmetic Mult) :: q ->
      stacks.(!current_stack) <- ar_mult stacks.(!current_stack);
      aux q
    | Exp (Arithmetic Divmod) :: q ->
      stacks.(!current_stack) <- ar_divmod stacks.(!current_stack);
      aux q
    | Exp (Arithmetic Max) :: q ->
      stacks.(!current_stack) <- ar_max stacks.(!current_stack);
      aux q
    | Exp (Intrinsic Drop) :: q ->
      stacks.(!current_stack) <- in_drop stacks.(!current_stack);
      aux q
    | Exp (Intrinsic Print) :: q ->
      stacks.(!current_stack) <- in_print stacks.(!current_stack);
      aux q
    | Exp (Intrinsic Dup) :: q ->
      stacks.(!current_stack) <- in_dup stacks.(!current_stack);
      aux q
    | Exp (Intrinsic Swap) :: q ->
      stacks.(!current_stack) <- in_swap stacks.(!current_stack);
      aux q
    | Exp (Intrinsic Over) :: q ->
      stacks.(!current_stack) <- in_over stacks.(!current_stack);
      aux q
    | Exp (Intrinsic Rot) :: q ->
      stacks.(!current_stack) <- in_rot stacks.(!current_stack);
      aux q
    | Exp (Comparator Geq) :: q ->
      stacks.(!current_stack) <- co_geq stacks.(!current_stack);
      aux q
    | Exp (Comparator G) :: q ->
      stacks.(!current_stack) <- co_g stacks.(!current_stack);
      aux q
    | Exp (Comparator Leq) :: q ->
      stacks.(!current_stack) <- co_leq stacks.(!current_stack);
      aux q
    | Exp (Comparator L) :: q ->
      stacks.(!current_stack) <- co_l stacks.(!current_stack);
      aux q
    | Exp (Comparator Eq) :: q ->
      stacks.(!current_stack) <- co_eq stacks.(!current_stack);
      aux q
    | Exp (Comparator Neq) :: q ->
      stacks.(!current_stack) <- co_neq stacks.(!current_stack);
      aux q
    | Exp (Gate Or) :: q ->
      stacks.(!current_stack) <- ga_or stacks.(!current_stack);
      aux q
    | Exp (Gate And) :: q ->
      stacks.(!current_stack) <- ga_and stacks.(!current_stack);
      aux q
    | Exp (Gate Not) :: q ->
      stacks.(!current_stack) <- ga_not stacks.(!current_stack);
      aux q
    | Exp (Deb DebugDump) :: q ->
      dump_stack stacks.(!current_stack);
      aux q
    | Exp (Declarator Var) :: Exp (Identifier stack_name) :: q ->
      let () =
        if not (is_valid_name stack_name names)
        then failwith ("stack name " ^ stack_name ^ " is already used.")
      in
      names.(!stack_count) <- stack_name;
      incr stack_count;
      aux q
    | Exp (Declarator Var) :: _ ->
      failwith "`var` declarator must be followed by a valid stack name."
    | Exp (Keyword In) :: Exp (Identifier stack_name) :: q ->
      (* let () = if is_valid_name stack_name names then failwith ("stack "^stack_name^" referenced before creation.") in *)
      current_stack := get_name_index stack_name names;
      aux q
    | Exp (Keyword In) :: _ ->
      failwith "`in` keyword must be followed by a valid stack name."
    | Exp (Identifier proc_name) :: q ->
      let proc_id = get_proc_id proc_name name_array in
      let proc_prog = proc_array.(proc_id) in
      aux (proc_prog @ q)
    | Exp (Intrinsic Length) :: q ->
      let p, s1 = pop stacks.(!current_stack) in
      stacks.(!current_stack) <- s1;
      let to_ret =
        match p with
        | Str str -> aux (Exp (Int (String.length str)) :: q)
        | _ -> failwith "Cannot get length of non-string values."
      in
      to_ret
    | Exp (Keyword Dot) :: q ->
      let p1, s1 = pop stacks.(!current_stack) in
      let p2, s2 = pop s1 in
      stacks.(!current_stack) <- s2;
      let to_ret =
        match p1, p2 with
        | Str str, Int a -> aux (Exp (Str (String.make 1 str.[a])) :: q)
        | _ -> failwith "Invalid arguments for `.`keyword."
      in
      to_ret
    | Exp (Keyword Transfer) :: Exp (Identifier stack1) :: q ->
      let id2 = get_name_index stack1 names in
      let id1 = !current_stack in
      let a1, s1 = pop stacks.(id1) in
      stacks.(id1) <- s1;
      stacks.(id2) <- push a1 (stacks.(id2));
      aux q
    | Exp (Keyword Transfer) :: _ -> failwith "Expected valid stack name after `transfer` keyword."
    | Exp (Keyword Dump) :: Exp (Identifier stack1) :: Exp (Identifier stack2) :: q ->
      let id1 = get_name_index stack1 names in
      let id2 = get_name_index stack2 names in
      stacks.(id2) <- append_stacks (stacks.(id1)) (stacks.(id2));
      stacks.(id1) <- Empty;
      aux q
    | Exp (Keyword Dump) :: _ -> failwith "Expected valid stack names after `dump` keyword."
    | Exp (Keyword Purge) :: Exp (Identifier stack_name) :: q ->
      let id = get_name_index stack_name names in
      stacks.(id) <- Empty;
      aux q
    | Exp (Keyword Purge) :: q-> failwith "Expected valid stack name after `purge` keyword."
    | Exp e :: q ->
      stacks.(!current_stack) <- push e stacks.(!current_stack);
      aux q
    | Sub (Main, a, _, _) :: q ->
      let _ = aux a in
      aux q
    | Sub (Cond, if_branch, else_branch, _) :: q ->
      let tok, s1 = pop stacks.(!current_stack) in
      stacks.(!current_stack) <- s1;
      let new_stack =
        match tok with
        | Bool true -> aux if_branch
        | Bool false -> aux else_branch
        | _ -> failwith "Expected `bool` type as head of the stack before `if-else`block."
      in
      stacks.(!current_stack) <- new_stack;
      aux q
    | Sub (Loop condition, prog, _, _) :: q ->
      let s1 = aux condition in
      let tok, s2 = pop s1 in
      stacks.(!current_stack) <- s2;
      (match tok with
      | Bool true ->
        let new_stack = aux prog in
        stacks.(!current_stack) <- new_stack;
        aux p
      | Bool false -> aux q
      | _ -> failwith "Expected `bool` type as head of the stack before `while` block.")
  in
  match p with
  | Sub (Main, a, _, _) -> aux a
  | _ -> failwith "Syntax error."
;;

let get_basic_main_prog () = Sub (Main, [ Exp (Identifier "main") ], [], false)

let interpret_file (filename : string) =
  let string_l = get_string_list_from_file filename in
  let tok_l = get_tok_l string_l in
  let _, proc_array, name_array = create_program_tree tok_l filename in
  let _ = eval_program (get_basic_main_prog ()) proc_array name_array in
  ()
;;

interpret_file "Examples/test_prog.plth";;
