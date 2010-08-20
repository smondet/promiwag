
(* Simple Typed Imperative Embedded Language *)

open Promiwag_std

module Definition = struct 

  type integer_type =
    | Type_uint8       
    | Type_uint16      
    | Type_uint32      
    | Type_uint64       
    | Type_uint_native


  type int_unary_operator =
    | Int_unary_minus     (* -  *)
    | Int_unary_plus      (* +  *)

  type int_binary_operator =
    | Int_binop_add   (*  +  *)
    | Int_binop_sub   (*  -  *)
    | Int_binop_mul   (*  *  *)
    | Int_binop_div   (*  /  *)
    | Int_binop_mod   (*  %  *)
    | Int_binop_bin_and  (*  &  *)
    | Int_binop_bin_or   (*  |  *)
    | Int_binop_bin_xor   (*  ^  *)
    | Int_binop_bin_shl   (*  << *) 
    | Int_binop_bin_shr   (*  >> *)

  type bool_binary_operator =
    | Bool_binop_equals
    | Bool_binop_notequals
    | Bool_binop_strictly_greater
    | Bool_binop_strictly_lower
    | Bool_binop_equal_or_greater
    | Bool_binop_equal_or_lower

  type variable_name = string

  type get_int_at_buffer =
    | Get_native of integer_type
    | Get_big_endian of integer_type
    | Get_little_endian of integer_type

  type buffer_type =
    | Type_sized_buffer of int
    | Type_pointer
    | Type_sizable_buffer of int_expression

  and int_expression =
    | Int_expr_unary of int_unary_operator * int_expression
    | Int_expr_binary of int_binary_operator * int_expression * int_expression
    | Int_expr_variable of variable_name
    | Int_expr_buffer_content of get_int_at_buffer * buffer_expression
    | Int_expr_literal of int64

  and buffer_expression =
    | Buffer_expr_variable of variable_name
    | Buffer_expr_offset of buffer_expression * int_expression

  and bool_expression =
    | Bool_expr_true        
    | Bool_expr_false       
    | Bool_expr_variable of variable_name
    | Bool_expr_and of bool_expression * bool_expression
    | Bool_expr_or of bool_expression * bool_expression
    | Bool_expr_not of bool_expression
    | Bool_expr_binary_int of bool_binary_operator * int_expression * int_expression

  type typed_expression =
    | Typed_int of integer_type * int_expression
    | Typed_bool of bool_expression
    | Typed_buffer of buffer_type * buffer_expression

  type typed_variable_kind =
    | Kind_int of integer_type
    | Kind_bool
    | Kind_buffer of buffer_type

  type typed_variable = {
    name: variable_name;
    kind: typed_variable_kind;
  }  

  type logic = 
    | Logic_bool_expr of bool_expression
    | Logic_variability of bool_expression * int_expression

  type statement_annotation = 
    | Annot_comment of string
    | Annot_specify of logic
    | Annot_alternative of [ `C | `Why ] * statement
  and statement =
    | Do_nothing
   (* | Do_comment of string *)
  (* | Do_int_evaluation of int_expression ---> expressions must keep
     purely functional hence, evaluation is useless *)
    | Do_block of statement list
    | Do_if of bool_expression * statement * statement
    | Do_while_loop of string * bool_expression * statement
    | Do_exit_while of string
    | Do_assignment  of variable_name * typed_expression
    | Do_external_assignment of variable_name * string * typed_expression list
    | Do_declaration of typed_variable
    | Do_log of string * typed_expression list
    | Do_annotated_statement of statement_annotation * statement


end

open Definition

module To_string = struct
    
  let spr = Printf.sprintf

  let  integer_type = function
    | Type_uint8       -> "uint8"      
    | Type_uint16      -> "uint16"     
    | Type_uint32      -> "uint32"     
    | Type_uint64      -> "uint64"     
    | Type_uint_native -> "uint_native"

  let int_unary_operator = function
    | Int_unary_minus             -> "-"
    | Int_unary_plus              -> "+"

  let int_binary_operator = function
    | Int_binop_add     ->   "+" 
    | Int_binop_sub     ->   "-" 
    | Int_binop_mul     ->   "*" 
    | Int_binop_div     ->   "/" 
    | Int_binop_mod     ->   "mod" 
    | Int_binop_bin_and ->   "bin_and" 
    | Int_binop_bin_or  ->   "bin_or" 
    | Int_binop_bin_xor ->   "bin_xor" 
    | Int_binop_bin_shl ->   "<<"  
    | Int_binop_bin_shr ->   ">>" 

  let bool_binary_operator = function
    | Bool_binop_equals            -> "=="   
    | Bool_binop_notequals         -> "!="      
    | Bool_binop_strictly_greater  -> ">"             
    | Bool_binop_strictly_lower    -> "<"           
    | Bool_binop_equal_or_greater  -> ">="             
    | Bool_binop_equal_or_lower    -> "<="           

  let get_int_at_buffer = function
    | Get_native         it -> spr "get-native-%s" (integer_type it)
    | Get_big_endian     it -> spr "get-bigend-%s" (integer_type it)
    | Get_little_endian  it -> spr "get-ltlend-%s" (integer_type it)

  let variable_name s = spr "[var:%s]" s

  let rec buffer_type = function
    | Type_sized_buffer   i -> spr "buffer-of-size-[%d]" i
    | Type_pointer          -> spr "pointer"
    | Type_sizable_buffer s -> spr "buffer-of-size-[%s]" (int_expression s)

  and int_expression = function
    | Int_expr_unary  (op, ex)        ->
      spr "(%s %s)" (int_unary_operator op) (int_expression ex)
    | Int_expr_binary (op, ea, eb)    -> 
      spr "(%s %s %s)" (int_expression ea)
        (int_binary_operator op) (int_expression eb)
    | Int_expr_variable  v            -> spr "%s" (variable_name v)
    | Int_expr_buffer_content (t, ex) ->
      spr "(%s @ %s)" (get_int_at_buffer t) (buffer_expression ex)
    | Int_expr_literal        i64     -> spr "%s" (Int64.to_string i64)

  and buffer_expression = function
    | Buffer_expr_variable v         -> spr "%s" (variable_name v)
    | Buffer_expr_offset (bex, iex)  -> 
      spr "(%s +> %s)" (buffer_expression bex) (int_expression iex)

  and bool_expression = function
    | Bool_expr_true   -> "True"     
    | Bool_expr_false  -> "False"     
    | Bool_expr_variable v -> spr "%s" (variable_name v)
    | Bool_expr_and        (a, b)     ->
      spr "(%s And %s)" (bool_expression a) (bool_expression b)
    | Bool_expr_or         (a, b)     ->
      spr "(%s Or %s)" (bool_expression a) (bool_expression b)
    | Bool_expr_not        ex         -> spr "(Not %s)" (bool_expression ex)
    | Bool_expr_binary_int (op, a, b) ->
      spr "(%s %s %s)" (int_expression a) 
        (bool_binary_operator op) (int_expression b)

  let typed_expression = function
    | Typed_int    (t, e) -> spr "[%s: %s]" (integer_type t) (int_expression e)
    | Typed_bool       e  -> spr "[bool: %s]" (bool_expression e)
    | Typed_buffer (t, e) -> spr "[%s: %s]" (buffer_type t) (buffer_expression e)

  let typed_variable_kind = function
    | Kind_int     t -> spr "%s" (integer_type t)
    | Kind_bool      ->     "bool"
    | Kind_buffer  t -> spr "%s" (buffer_type t)

  let logic ?(before="") = function
    | Logic_bool_expr b -> 
      spr "%sLogic: %s" before (bool_expression b)
    | Logic_variability (i,v) ->
      spr "%sInvariant: %s\n%sVariant: %s" 
      before (bool_expression i) before (int_expression v)

  let rec statement_annotation ?(before="") = function
    | Annot_comment s -> spr "%s%s" before s
    | Annot_specify l -> logic ~before l
    | Annot_alternative (`C, s) -> 
      spr "%sC: %s" before (statement s)
    | Annot_alternative (`Why, s) -> 
      spr "%sWhy: %s" before (statement s)
  and statement ?(indent=0) =
    let cur_indent = String.make indent ' ' in
    let cat = String.concat "" in
    let catmap f l = cat (List.map f l) in
    let assign a b = spr "%s%s := %s;\n" cur_indent a b in
    let prev_indent = indent in
    let indent = indent + 2 in
    function 
    | Do_nothing                 -> spr "%sNop;\n" cur_indent
    (* | Do_comment        s        -> spr "%sComment: %s;\n" cur_indent s *)
    | Do_block          e        -> 
      spr "%s{\n%s%s}\n" cur_indent (catmap (statement ~indent) e) cur_indent
    | Do_if            (e, a, b) ->
      spr "%sIf [%s] Then\n%s%sElse\n%s\n"
        cur_indent (bool_expression e) (statement ~indent a) 
        cur_indent (statement ~indent b)
    | Do_while_loop    (s, e, a)    -> 
      spr "%sWhile:\"%s\" [%s] Do\n%s\n" 
        cur_indent s (bool_expression e) (statement ~indent a)
    | Do_exit_while s -> spr "%sExit While %s;\n" cur_indent s
    | Do_assignment  (a, b) -> assign (variable_name    a) (typed_expression b)
    | Do_external_assignment  (a, f, args) ->
      assign (variable_name a) 
        (sprintf "external:%s(%s)" f
           (Str.concat ", " (Ls.map typed_expression args)))
    | Do_declaration t -> 
      spr "%sDeclare %s of type: %s;\n" cur_indent
        (variable_name t.name) (typed_variable_kind t.kind)
    | Do_log (f, l) ->
      spr "%sLog (format: %S) [%s];\n" cur_indent f 
        (Str.concat "; " (Ls.map typed_expression l))
    | Do_annotated_statement (annot, st) ->
      spr "%s\n%s"
        (statement_annotation ~before:(spr "%s# " cur_indent) annot)
        (statement ~indent:prev_indent st)

end



module Expression = struct

  let fail s = failwith (sprintf "STIEL.Expr: %s" s)
  let fail_e s e = fail (sprintf "%s: %s" s (To_string.typed_expression e))
  let fail_2e s a b =
    fail (sprintf "%s: %s, %s" s
            (To_string.typed_expression a) (To_string.typed_expression b))

  let int_expression = function
    | Typed_int (_, e) -> e
    | e -> fail_e "This typed expression is not an integer one" e
  let buffer_expression = function
    | Typed_buffer (_, e) -> e
    | e -> fail_e "This typed expression is not a buffer one" e

  let bool_expression = function
    | Typed_bool e -> e
    | e -> fail_e "This typed expression is not an boolean one" e

  let int = int_expression
  let buffer = buffer_expression
  let bool = bool_expression

  let u8    e = Typed_int (Type_uint8      , Int_expr_literal (Int64.of_int e))
  let u16   e = Typed_int (Type_uint16     , Int_expr_literal (Int64.of_int e))
  let u32   e = Typed_int (Type_uint32     , Int_expr_literal (Int64.of_int e))
  let u64   e = Typed_int (Type_uint64     , Int_expr_literal (Int64.of_int e))
  let unat  e = Typed_int (Type_uint_native, Int_expr_literal (Int64.of_int e))

  let i64_to_u8    e = Typed_int (Type_uint8      , Int_expr_literal  e)
  let i64_to_u16   e = Typed_int (Type_uint16     , Int_expr_literal  e)
  let i64_to_u32   e = Typed_int (Type_uint32     , Int_expr_literal  e)
  let i64_to_u64   e = Typed_int (Type_uint64     , Int_expr_literal  e)
  let i64_to_unat  e = Typed_int (Type_uint_native, Int_expr_literal  e)

  let type1 = function
    | Typed_int (Type_uint8      , _) -> (fun e -> Typed_int (Type_uint8      , e))
    | Typed_int (Type_uint16     , _) -> (fun e -> Typed_int (Type_uint16     , e))
    | Typed_int (Type_uint32     , _) -> (fun e -> Typed_int (Type_uint32     , e))
    | Typed_int (Type_uint64     , _) -> (fun e -> Typed_int (Type_uint64     , e))
    | Typed_int (Type_uint_native, _) -> (fun e -> Typed_int (Type_uint_native, e))
    | e -> fail_e "type1: type not supported" e

  let type2 a b =
    match a,b with
    | Typed_int (Type_uint8      , _), Typed_int (Type_uint8      , _) -> type1 a
    | Typed_int (Type_uint16     , _), Typed_int (Type_uint16     , _) -> type1 a
    | Typed_int (Type_uint32     , _), Typed_int (Type_uint32     , _) -> type1 a
    | Typed_int (Type_uint64     , _), Typed_int (Type_uint64     , _) -> type1 a
    | Typed_int (Type_uint_native, _), Typed_int (Type_uint_native, _) -> type1 a
    | _ -> (fun e ->
      fail_2e (sprintf "type2: in %s mismatch or not supported"
                 (To_string.int_expression e)) a b)

  let to_unat = function
    | Typed_int (Type_uint8      , e) -> Typed_int (Type_uint_native, e)
    | Typed_int (Type_uint16     , e) -> Typed_int (Type_uint_native, e)
    | Typed_int (Type_uint32     , e) -> Typed_int (Type_uint_native, e)
    | Typed_int (Type_uint64     , e) -> Typed_int (Type_uint_native, e)
    | Typed_int (Type_uint_native, e) -> Typed_int (Type_uint_native, e)
    | e -> fail_e "to_unat: type not supported" e

  let minus m = type1 m $ Int_expr_unary (Int_unary_minus, int m)
  let add a b =     type2 a b $ Int_expr_binary (Int_binop_add, int a, int b)
  let sub a b =     type2 a b $ Int_expr_binary (Int_binop_sub, int a, int b)
  let mul a b =     type2 a b $ Int_expr_binary (Int_binop_mul, int a, int b)
  let div a b    =  type1 a   $ Int_expr_binary (Int_binop_div, int a, int b)
  let modulo a b =  type1 a   $ Int_expr_binary (Int_binop_mod, int a, int b)
  let bin_and a b = type2 a b $ Int_expr_binary (Int_binop_bin_and, int a, int b)
  let bin_or  a b = type2 a b $ Int_expr_binary (Int_binop_bin_or , int a, int b)
  let bin_xor a b = type2 a b $ Int_expr_binary (Int_binop_bin_xor, int a, int b)
  let bin_shl a b = type1 a   $ Int_expr_binary (Int_binop_bin_shl, int a, int b)
  let bin_shr a b = type1 a   $ Int_expr_binary (Int_binop_bin_shr, int a, int b)
  let get_style s t = match s with
    | `big -> Get_big_endian t
    | `little -> Get_little_endian t
    | `native -> Get_native t
  let u8_at   ?(how=`big) b =
    Typed_int (Type_uint8       ,
               Int_expr_buffer_content (get_style how Type_uint8       , buffer b))
  let u16_at  ?(how=`big) b =
    Typed_int (Type_uint16      ,
               Int_expr_buffer_content (get_style how Type_uint16      , buffer b))
  let u32_at  ?(how=`big) b =
    Typed_int (Type_uint32      ,
               Int_expr_buffer_content (get_style how Type_uint32      , buffer b))
  let u64_at  ?(how=`big) b =
    Typed_int (Type_uint64      ,
               Int_expr_buffer_content (get_style how Type_uint64      , buffer b))
  let unat_at ?(how=`big) b =
    Typed_int (Type_uint_native ,
               Int_expr_buffer_content (get_style how Type_uint_native , buffer b))

  let fitted_uint_type_and_size ?(fail=failwith) i =
    if 1 <= i && i <= 8 then         (Type_uint8, 8)
    else if   9 <= i && i <= 16 then (Type_uint16, 16)
    else if  17 <= i && i <= 32 then (Type_uint32, 32)
    else if  33 <= i && i <= 64 then (Type_uint64, 64)
    else 
      failwith (sprintf "fitted_uint: too long integer: %d" i)

  let ufitted ~size e =
    match size with
    | 8  -> i64_to_u8  e
    | 16 -> i64_to_u16 e
    | 32 -> i64_to_u32 e
    | 64 -> i64_to_u64 e
    | _ -> failwith "ufitted: size not in {8, 16, 32, 64}"

  let ufitted_at ~size ~how b =
    match size with
    | 8  ->  u8_at ~how b
    | 16 -> u16_at ~how b
    | 32 -> u32_at ~how b
    | 64 -> u64_at ~how b
    | _ -> failwith "ufitted_at: size not in {8, 16, 32, 64}"

  let ones size =
    if Int64.to_float Int64.max_int < (2. ** (float size)) then
      failwith "ones: Too big int64 !"
    else
      (Int64.sub (Int64.shift_left 1L size) 1L)



  let t = Typed_bool Bool_expr_true
  let f = Typed_bool Bool_expr_false
  let band a b = Typed_bool (Bool_expr_and (bool a, bool b))
  let bor  a b = Typed_bool (Bool_expr_or  (bool a, bool b))
  let bnot  a   = Typed_bool (Bool_expr_not (bool a))
  let eq  a b = Typed_bool (Bool_expr_binary_int (Bool_binop_equals           , int a, int b))
  let neq a b = Typed_bool (Bool_expr_binary_int (Bool_binop_notequals        , int a, int b))
  let gt  a b = Typed_bool (Bool_expr_binary_int (Bool_binop_strictly_greater , int a, int b))
  let lt  a b = Typed_bool (Bool_expr_binary_int (Bool_binop_strictly_lower   , int a, int b))
  let ge  a b = Typed_bool (Bool_expr_binary_int (Bool_binop_equal_or_greater , int a, int b))
  let le  a b = Typed_bool (Bool_expr_binary_int (Bool_binop_equal_or_lower   , int a, int b))

  let rec ls_and  = function
    | [] -> t
    | [a] -> a
    | [a ; b] -> band a b
    | h :: t -> band h (ls_and t)
  let rec ls_or  = function
    | [] -> f
    | [a] -> a
    | [a ; b] -> bor a b
    | h :: t -> bor h (ls_or t)

  let offset b i =
    let t = 
      match b with
      | Typed_buffer (Type_pointer,  _) ->
        (fun e -> Typed_buffer (Type_pointer , e))
      | Typed_buffer (Type_sized_buffer old_i, _) ->
        let new_size =
          Int_expr_binary (Int_binop_sub,
                           Int_expr_literal (Int64.of_int old_i), int i) in
        (fun e -> Typed_buffer (Type_sizable_buffer new_size, e))
      | Typed_buffer (Type_sizable_buffer old_i, _) ->
        let new_size = Int_expr_binary (Int_binop_sub, old_i, int i) in
        (fun e -> Typed_buffer (Type_sizable_buffer new_size, e))
      | _ -> failwith "offset: Cannot handle this type"
    in
    t (Buffer_expr_offset (buffer b, int i))

end
module Variable = struct

  let typed_variable ?(unique=true) name kind =
    { name = if unique then Unique.name name else name;
      kind = kind}

  let name tv = tv.name
  let kind tv = tv.kind

  let tv = typed_variable

  let sized_buffer ?unique i v = tv ?unique v $ Kind_buffer (Type_sized_buffer i)
  let pointer      ?unique   v = tv ?unique v $ Kind_buffer Type_pointer           
  let u8           ?unique   v = tv ?unique v $ Kind_int    Type_uint8             
  let u16          ?unique   v = tv ?unique v $ Kind_int    Type_uint16            
  let u32          ?unique   v = tv ?unique v $ Kind_int    Type_uint32            
  let u64          ?unique   v = tv ?unique v $ Kind_int    Type_uint64            
  let unat         ?unique   v = tv ?unique v $ Kind_int    Type_uint_native       
  let bool         ?unique   v = tv ?unique v $ Kind_bool   

  let expression tv =
    match tv.kind with
    | Kind_bool -> Typed_bool (Bool_expr_variable tv.name)
    | Kind_int t -> Typed_int (t, Int_expr_variable tv.name)
    | Kind_buffer t -> Typed_buffer (t, Buffer_expr_variable tv.name)

  let int_expression tv = Expression.int_expression (expression tv)
  let bool_expression tv = Expression.bool_expression (expression tv)
  let buffer_expression tv = Expression.buffer_expression (expression tv)

  let kind_of_expr = function
    | Typed_bool _        -> Kind_bool 
    | Typed_int (t, _)    -> Kind_int t
    | Typed_buffer (t, _) -> Kind_buffer t

  let assign v te = Do_assignment (v.name, te)

  let ext_assign v f tel = Do_external_assignment (v.name, f, tel)

  let declare v = Do_declaration v

  let declare_and_assign v te = 
    [declare v; assign v te]
      
end

module Statement = struct

  let declaration n t = Do_declaration (Variable.tv ~unique:false n t)

  let nop = Do_nothing

  let block l = Do_block l
  let unblock = function
    | Do_block l -> l
    | s -> [s]

  let conditional ?(statement_then=Do_nothing) ?(statement_else=Do_nothing)
      condition =
    Do_if (Expression.bool condition, statement_then, statement_else)
      
  let while_loop ?(name="") c s = Do_while_loop (name, Expression.bool c, s)
  let exit_while = Do_exit_while ""
  let exit_named_while s = Do_exit_while s

  let cmt s = Do_annotated_statement (Annot_comment s, Do_nothing)


  let meta_log user_printers format te_list =
    let double_escape = "AROBASEESCAPE4732992387737238292933" in

    let simple_escape =
      sprintf "9sdfa9dsg3gfds8gfdsggfdsgfdsgfdsgfdsgfdsgfdsgfds33%s" in
    let standard_printers = [
      ("@hex", simple_escape "hex", fun te -> [te]);
      ("@int", simple_escape "int", fun te -> [te]);
      ("@expr",simple_escape "expr", fun te -> [te]); ] in
    let all_printers =
      standard_printers @ 
        (Ls.map user_printers ~f:(fun (a,b,c) ->
          let newb =
            Str.multi_replace ~str:b ~sub:"@" ~by:(simple_escape "") in
          (a, newb, c))) in
    let unescape str =
      let str =
        (Str.multi_replace ~str ~sub:(simple_escape "") ~by:"@") in
      Str.multi_replace ~str ~sub:double_escape ~by:"@@" in

    let first_pattern fmt =
      let try_the_patterns =
        Ls.map all_printers ~f:(fun ((pat, replace, te_fun) as printer) ->
          let index = try Str.find fmt pat with e -> max_int in
          (index, printer)) in
      let _, the_first =
        Ls.fold_left ~f:(fun (cur_index, cur_prt) (index, prt) ->
          if cur_index > index then (index, Some prt)
          else (cur_index, cur_prt))
          ~init:(max_int, None) try_the_patterns in
      the_first
    in
    let rec parse_string te_list_list te_list str =
      match first_pattern str, te_list with
      | None, [] -> (str, Ls.flatten (Ls.rev te_list_list))
      | Some (sub, by, te_fun), te :: next_tes ->
        (parse_string ((te_fun te) :: te_list_list)
           next_tes (snd (Str.replace ~str ~sub ~by)))
      | None, l ->
        failwith 
          (sprintf "meta_log: too many arguments (%d) and no more patterns: %S"
             (Ls.length l) (unescape str))
      | Some (sub, by, _), [] ->
        failwith
          (sprintf "meta_log: not enough arguments for all the patterns: \
                    '%s' in %S"
             sub (unescape str))
    in
    let fmt = Str.multi_replace ~str:format ~sub:"@@" ~by:double_escape in
    let new_format, new_args = parse_string [] te_list fmt in

    let actually_the_format = unescape new_format in

    Do_log (actually_the_format, new_args)

  let log f l = meta_log [] f l

  let switch_int expr cases =
    Ls.map cases ~f:(fun (i, statement_then) ->
      conditional (Expression.eq expr (Expression.unat i)) ~statement_then)

end

module Annotated_statement = struct

  let cmt c s = Do_annotated_statement (Annot_comment c, s)

  let why w s = 
    Do_annotated_statement (Annot_alternative (`Why, w), s)

  let variability inv var s =
    Do_annotated_statement 
      (Annot_specify (Logic_variability 
                        (Expression.bool inv, Expression.int var)), s)

end


module Transform = struct


  module Partial_evaluation = struct
    type environment = {
      variables:  (variable_name, typed_expression) Environment.t;
      use_purity: bool;
      do_symbolic_equality: bool;
    }

    (*  WARNING:
        do_symbolic_equality and use_purity will absorb some side effects.

        For now side effects are just the divisions/modulos by zero.
    *)


    module Env = Environment

    let environment
        ?(do_symbolic_equality=false) ?(use_purity=false) ?variables () =
      {variables = (match variables with | Some s -> s | None -> Env.empty);
       use_purity = use_purity;
       do_symbolic_equality = do_symbolic_equality;}

    exception Division_by_zero_in of int_expression

    let int_unary_operator environment = function
      | Int_unary_minus             -> Int64.neg
      | Int_unary_plus              -> fun i -> i

    let int_binary_operator environment = function
      | Int_binop_add     -> Int64.add
      | Int_binop_sub     -> Int64.sub
      | Int_binop_mul     -> Int64.mul
      | Int_binop_div     -> Int64.div
      | Int_binop_mod     -> Int64.rem
      | Int_binop_bin_and -> Int64.logand
      | Int_binop_bin_or  -> Int64.logor
      | Int_binop_bin_xor -> Int64.logxor
      | Int_binop_bin_shl -> (fun a b -> Int64.shift_left a (Int64.to_int b))
      | Int_binop_bin_shr -> (fun a b -> Int64.shift_right_logical a (Int64.to_int b))
      
    let transform_int_binop environment op a b =
      let try_commutative_with_zero op e dont_know =
        match op with
        | Int_binop_add | Int_binop_bin_or -> e
        | Int_binop_mul | Int_binop_bin_and -> Int_expr_literal Int64.zero
        | _ -> dont_know
      in
      let try_with_zero_on_the_right op e dont_know =
        match op with
        | Int_binop_sub | Int_binop_bin_shl | Int_binop_bin_shr -> e
        | Int_binop_div | Int_binop_mod ->
          raise (Division_by_zero_in dont_know)
        | _ -> dont_know
      in
      let try_with_zero_on_the_left op e dont_know =
        match op with
        | Int_binop_div | Int_binop_mod -> Int_expr_literal Int64.zero
        | _ -> dont_know
      in
      let try_commutative_with_one op e dont_know =
        match op with
        | Int_binop_mul -> e
        | _ -> dont_know
      in
      let try_with_one_on_the_right op e dont_know =
        match op with
        | Int_binop_div -> e
        | Int_binop_mod -> Int_expr_literal Int64.zero
        | _ -> dont_know
      in
      let try_symbolic op a b dont_know =
        match a = b, op with
        | true, Int_binop_sub
        | true, Int_binop_bin_xor -> Int_expr_literal Int64.zero
        | true, Int_binop_add ->
          Int_expr_binary (Int_binop_mul,
                           Int_expr_literal (Int64.of_int 2), a)
        | true, Int_binop_div -> Int_expr_literal Int64.one 
        | true, Int_binop_mod -> Int_expr_literal Int64.zero
        | true, Int_binop_bin_and 
        | true, Int_binop_bin_or -> a
        | _ -> dont_know
      in
      let original = Int_expr_binary (op, a, b) in
      let use_purity = environment.use_purity in
      begin match a, b with
      | Int_expr_literal i64a, Int_expr_literal i64b -> 
        Int_expr_literal ((int_binary_operator environment op) i64a i64b)
      | Int_expr_literal i64a, pb when i64a = Int64.zero && use_purity -> 
        try_commutative_with_zero op pb original
      | pa, Int_expr_literal i64b when i64b = Int64.zero && use_purity ->
        let after_commut =
          try_commutative_with_zero op pa original in
        let after_right_zero = 
          try_with_zero_on_the_right op pa after_commut in
        try_with_zero_on_the_left op pa after_right_zero
      | Int_expr_literal i64a, pb when i64a = Int64.one && use_purity -> 
        try_commutative_with_one op pb original
      | pa, Int_expr_literal i64b when i64b = Int64.one && use_purity ->
        let after_commut =
          try_commutative_with_one op pa original in
        try_with_one_on_the_right op pa after_commut
      | pa, pb ->
        if environment.do_symbolic_equality && use_purity then
          try_symbolic op pa pb (Int_expr_binary (op, pa, pb))
        else
          Int_expr_binary (op, pa, pb)
      end


    let bool_binary_operator environment = function
      | Bool_binop_equals            -> (=)
      | Bool_binop_notequals         -> (<>)
      | Bool_binop_strictly_greater  -> (>)
      | Bool_binop_strictly_lower    -> (<)
      | Bool_binop_equal_or_greater  -> (>=)
      | Bool_binop_equal_or_lower    -> (<=)

    let rec int_variable environment v =
      match Env.find_opt environment.variables v with
      | None -> Int_expr_variable v
      | Some e -> int_expression environment (Expression.int e)
    and bool_variable environment v =
      match Env.find_opt environment.variables v with
      | None -> Bool_expr_variable v
      | Some e -> bool_expression environment (Expression.bool e)
    and buffer_variable environment v =
      match Env.find_opt environment.variables v with
      | None -> Buffer_expr_variable v
      | Some e -> buffer_expression environment (Expression.buffer e)
    and buffer_type environment = function
      | Type_sized_buffer   i -> Type_sized_buffer   i
      | Type_pointer          -> Type_pointer         
      | Type_sizable_buffer s -> Type_sizable_buffer s

    and int_expression environment = function
      | Int_expr_unary  (op, ex)        ->
        begin match int_expression environment ex with
        | Int_expr_literal i64 -> 
          Int_expr_literal ((int_unary_operator environment op) i64)
        | pe -> Int_expr_unary  (op, pe)
        end
      | Int_expr_binary (op, ea, eb) -> 
        transform_int_binop environment op
          (int_expression environment ea) (int_expression environment eb)
      | Int_expr_variable v  -> int_variable environment v
      | Int_expr_buffer_content (_, _) as original -> original
      | (Int_expr_literal _) as original -> original

    and buffer_expression environment = function
      | Buffer_expr_variable v -> buffer_variable environment v
      | Buffer_expr_offset (_, _) as original -> original
        
    and bool_expression environment expr =
      let allowed e =
        if environment.use_purity then
          true
        else
          (e = Bool_expr_true) || (e = Bool_expr_false) in
      match expr with
      | Bool_expr_true   -> Bool_expr_true 
      | Bool_expr_false  -> Bool_expr_false
      | Bool_expr_variable v  -> bool_variable environment v
      | Bool_expr_and (a, b) ->
        let propa = bool_expression environment a in
        let propb = bool_expression environment b in
        begin match propa, propb with
        | Bool_expr_true, anything  when allowed anything -> anything
        | anything, Bool_expr_true  when allowed anything -> anything
        | Bool_expr_false, anything when allowed anything -> Bool_expr_false
        | anything, Bool_expr_false when allowed anything -> Bool_expr_false
        | pa, pb -> Bool_expr_and (pa, pb)
        end
      | Bool_expr_or         (a, b)     ->
        let propa = bool_expression environment a in
        let propb = bool_expression environment b in
        begin match propa, propb with
        | Bool_expr_true, anything  when allowed anything -> Bool_expr_true
        | anything, Bool_expr_true  when allowed anything -> Bool_expr_true
        | Bool_expr_false, anything when allowed anything -> anything
        | anything, Bool_expr_false when allowed anything -> anything
        | pa, pb -> Bool_expr_or (pa, pb)
        end
      | Bool_expr_not        ex         ->
        begin match bool_expression environment ex with
        | Bool_expr_true  -> Bool_expr_false
        | Bool_expr_false -> Bool_expr_true
        | pex -> Bool_expr_not pex
        end
      | Bool_expr_binary_int (op, a, b) ->
        let comp = bool_binary_operator environment op in
        let propa = int_expression environment a in
        let propb = int_expression environment b in
        begin match propa, propb with
        | Int_expr_literal a64, Int_expr_literal b64 ->
          if comp a64 b64 then Bool_expr_true else Bool_expr_false
        | pa, pb -> 
          if op =  Bool_binop_equals &&
            environment.use_purity &&
            environment.do_symbolic_equality
          then
            if pa = pb then Bool_expr_true else Bool_expr_binary_int (op, pa, pb)
          else
            Bool_expr_binary_int (op, pa, pb)
        end

    let typed_expression compiler = function 
      | Typed_int    (t, e) -> Typed_int    (t, int_expression compiler e)
      | Typed_bool       e  -> Typed_bool   (bool_expression compiler e)
      | Typed_buffer (t, e) -> Typed_buffer (t, buffer_expression compiler e)

  end

  let propagate_constants_in_int
      ?(use_purity=false) ?(do_symbolic_equality=false) expr =
    let env = 
      Partial_evaluation.environment ~do_symbolic_equality ~use_purity () in
    Partial_evaluation.int_expression env expr
      
  let propagate_constants_in_bool
      ?(use_purity=false) ?(do_symbolic_equality=false) expr =
    let env = 
      Partial_evaluation.environment ~do_symbolic_equality ~use_purity () in
    Partial_evaluation.bool_expression env expr


end



module Visit = struct

  type compiler = {
    on_variables: string -> unit;
    on_variable_declarations: string -> unit;
  }
  let nop = fun (s:string) -> ()

  let compiler ?(on_variables=nop) ?(on_variable_declarations=nop) () =
    { on_variables = on_variables;
      on_variable_declarations = on_variable_declarations }

  let  integer_type compiler = function
    | Type_uint8       -> ()
    | Type_uint16      -> ()  
    | Type_uint32      -> ()
    | Type_uint64      -> ()
    | Type_uint_native -> ()

  let int_unary_operator compiler = function
    | Int_unary_minus             -> ()
    | Int_unary_plus              -> ()

  let int_binary_operator compiler = function
    | Int_binop_add     -> ()
    | Int_binop_sub     -> ()
    | Int_binop_mul     -> ()
    | Int_binop_div     -> ()
    | Int_binop_mod     -> ()
    | Int_binop_bin_and -> ()
    | Int_binop_bin_or  -> ()
    | Int_binop_bin_xor -> ()
    | Int_binop_bin_shl -> ()
    | Int_binop_bin_shr -> ()
      
  let bool_binary_operator compiler = function
    | Bool_binop_equals            -> ()
    | Bool_binop_notequals         -> ()
    | Bool_binop_strictly_greater  -> ()
    | Bool_binop_strictly_lower    -> ()
    | Bool_binop_equal_or_greater  -> ()
    | Bool_binop_equal_or_lower    -> ()

  let variable_name compiler s = compiler.on_variables s

  let rec buffer_type compiler = function
    | Type_sized_buffer   i -> ()
    | Type_pointer          -> ()
    | Type_sizable_buffer s -> ()

  and int_expression compiler = function
    | Int_expr_unary  (op, ex)        ->
      ignore (int_unary_operator compiler op, int_expression compiler ex)
    | Int_expr_binary (op, ea, eb)    -> 
      ignore (int_binary_operator compiler op,
              int_expression compiler ea, int_expression compiler eb)
    | Int_expr_variable  v            -> (variable_name compiler v)
    | Int_expr_buffer_content (t, ex) -> (buffer_expression compiler ex)
    | Int_expr_literal        i64     -> ()

  and buffer_expression compiler = function
    | Buffer_expr_variable v         -> (variable_name compiler v)
    | Buffer_expr_offset (bex, iex)  -> 
      ignore (buffer_expression compiler bex, int_expression compiler iex)

  and bool_expression compiler = function
    | Bool_expr_true   -> ()
    | Bool_expr_false  -> ()
    | Bool_expr_variable v -> (variable_name compiler v)
    | Bool_expr_and        (a, b)     ->
      ignore (bool_expression compiler a, bool_expression compiler b)
    | Bool_expr_or         (a, b)     ->
      ignore (bool_expression compiler a, bool_expression compiler b)
    | Bool_expr_not        ex         ->
      ignore (bool_expression compiler ex)
    | Bool_expr_binary_int (op, a, b) ->
      ignore (bool_binary_operator compiler op,
              int_expression compiler a, int_expression compiler b)

  let typed_expression compiler = function 
    | Typed_int    (t, e) -> 
      ignore (integer_type compiler t, int_expression compiler e)
    | Typed_bool       e  -> 
      ignore (bool_expression compiler e)
    | Typed_buffer (t, e) ->
      ignore (buffer_type compiler Type_pointer, 
              buffer_expression compiler e)
      
  let typed_variable_kind compiler = function
    | Kind_int    t -> integer_type compiler t
    | Kind_bool     -> ()
    | Kind_buffer t -> buffer_type compiler t

  let rec statement compiler =
    function 
    | Do_nothing                 -> ()
    | Do_block          e        -> 
      Ls.iter (statement compiler) e
    | Do_if            (e, a, b) ->
      ignore (bool_expression compiler e,
              statement compiler a, statement compiler b)
    | Do_while_loop    (s, e, a)    -> 
      ignore (bool_expression compiler e, statement compiler a)
    | Do_exit_while s -> ()
    | Do_assignment  (a, b) -> 
      ignore (variable_name compiler a, typed_expression compiler b)
    | Do_external_assignment  (a, f, args) -> 
      ignore (variable_name compiler a,
              Ls.map (typed_expression compiler) args)
    | Do_declaration t -> 
      compiler.on_variable_declarations t.name;
      ignore (variable_name compiler t.name,
              typed_variable_kind compiler t.kind)
    | Do_log (f, l) ->
      ignore (Ls.map (typed_expression compiler) l)
    | Do_annotated_statement (a, s) -> statement compiler s

end






module Verify = struct

  type problem =
    | Ok
    | Double_defined_variables of string list
        
  let check_block_for_double_vriables compiler stlist =
    let vars = ref [] in
    Ls.iter stlist ~f:(function
      | Do_declaration t -> vars := t.name :: !vars;
      | _ -> ());
    let nb_vars = Ls.length !vars in
    let nb_unique_vars = Ls.length (Ls.unique !vars) in
    if nb_vars = nb_unique_vars then
      Ok
    else
      Double_defined_variables !vars

  let rec block compiler = function
    | Do_block          e        ->
      check_block_for_double_vriables compiler e
    | _ -> Ok


end



module Standard_renaming = struct

  module STIEL = Definition
  module Expr = Expression
  module Var = Variable
  module Do = Statement
  module Annot = Annotated_statement
  module Stiel_to_str = To_string
end




