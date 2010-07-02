
(* Simple Typed Imperative Embedded Language *)

type integer_type =
  | Type_uint8
  | Type_uint16
  | Type_uint32
  | Type_uint64
  | Type_uint_native


type int_unary_operator =
  | Int_unary_minus     (* -  *)
  | Int_unary_plus      (* +  *)
  | Int_unary_big2little_endian
  | Int_unary_little2big_endian

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


type int_variable = string

type bool_variable = string

type buffer_variable = string

type buffer_type =
  | Type_sized_buffer of int
  | Type_pointer
  | Type_sizable_buffer of int_expression

and int_expression =
  | Int_expr_unary of int_unary_operator * int_expression
  | Int_expr_binary of int_binary_operator * int_expression * int_expression
  | Int_expr_variable of int_variable
  | Int_expr_buffer_content of integer_type * buffer_expression
  | Int_expr_literal of int64

and buffer_expression =
  | Buf_expr_variable of buffer_variable
  | Buf_expr_offset of buffer_expression * int_expression

and bool_expression =
  | Bool_expr_true        
  | Bool_expr_false       
  | Bool_expr_and of bool_expression * bool_expression
  | Bool_expr_or of bool_expression * bool_expression
  | Bool_expr_not of bool_expression
  | Bool_expr_binary_int of bool_binary_operator * int_expression * int_expression

type statement =
  | Do_nothing
  (* | Do_int_evaluation of int_expression ---> expressions must keep
     purely functional hence, evaluation is useless *)
  | Do_block of statement list
  | Do_if of bool_expression * statement * statement
  | Do_while_loop of bool_expression * statement
  | Do_assign_int of int_variable * int_expression
  | Do_assign_buffer of buffer_variable * buffer_expression
  | Do_assign_bool of bool_variable * bool_expression

module To_string = struct
    
  let spr = Printf.sprintf

  let  integer_type = function
    | Type_uint8       -> "uint8"      
    | Type_uint16      -> "uint16"     
    | Type_uint32      -> "uint32"     
    | Type_uint64      -> "uint64"     
    | Type_uint_native -> "uint_native"

  let int_unary_operator = function
  | Int_unary_minus             -> "+"
  | Int_unary_plus              -> "-"
  | Int_unary_big2little_endian -> "big2ltl"
  | Int_unary_little2big_endian -> "ltl2big"

  let int_binary_operator = function
    | Int_binop_add     ->   "+" 
    | Int_binop_sub     ->   "-" 
    | Int_binop_mul     ->   "*" 
    | Int_binop_div     ->   "/" 
    | Int_binop_mod     ->   "mod" 
    | Int_binop_bin_and ->   "bin_and" 
    | Int_binop_bin_or  ->   "bin_or" 
    | Int_binop_bin_xor ->   "bon_xor" 
    | Int_binop_bin_shl ->   "<<"  
    | Int_binop_bin_shr ->   ">>" 

  let bool_binary_operator = function
    | Bool_binop_equals            -> "=="   
    | Bool_binop_notequals         -> "!="      
    | Bool_binop_strictly_greater  -> ">"             
    | Bool_binop_strictly_lower    -> "<"           
    | Bool_binop_equal_or_greater  -> "<="             
    | Bool_binop_equal_or_lower    -> ">="           


  let int_variable    s = s
  let bool_variable   s = s
  let buffer_variable s = s

  let rec buffer_type = function
    | Type_sized_buffer   i -> spr "buffer[%d]" i
    | Type_pointer          -> spr "pointer"
    | Type_sizable_buffer s -> spr "buffer[%s]" (int_expression s)

  and int_expression = function
    | Int_expr_unary  (op, ex)        ->
      spr "(%s %s)" (int_unary_operator op) (int_expression ex)
    | Int_expr_binary (op, ea, eb)    -> 
      spr "(%s %s %s)" (int_expression ea)
        (int_binary_operator op) (int_expression eb)
    | Int_expr_variable  v            -> spr "%s" (int_variable v)
    | Int_expr_buffer_content (t, ex) ->
      spr "(%s @ %s)" (integer_type t) (buffer_expression ex)
    | Int_expr_literal        i64     -> spr "%s" (Int64.to_string i64)

  and buffer_expression = function
    | Buf_expr_variable v         -> spr "%s" (buffer_variable v)
    | Buf_expr_offset (bex, iex)  -> 
      spr "(%s +> %s)" (buffer_expression bex) (int_expression iex)

  and bool_expression = function
    | Bool_expr_true   -> "True"     
    | Bool_expr_false  -> "False"     
    | Bool_expr_and        (a, b)     ->
      spr "(%s And %s)" (bool_expression a) (bool_expression b)
    | Bool_expr_or         (a, b)     ->
      spr "(%s Or %s)" (bool_expression a) (bool_expression b)
    | Bool_expr_not        ex         -> spr "(Not %s)" (bool_expression ex)
    | Bool_expr_binary_int (op, a, b) ->
      spr "(%s %s %s)" (int_expression a) 
        (bool_binary_operator op) (int_expression b)

  let rec statement ?(indent=0) =
    let cur_indent = String.make indent ' ' in
    let cat = String.concat "" in
    let catmap f l = cat (List.map f l) in
    let assign a b = spr "%s%s := %s;\n" cur_indent a b in
    let indent = indent + 2 in
    function 
    | Do_nothing                 -> spr "%sNop;\n" cur_indent
    | Do_block          e        -> 
      spr "%s{\n%s%s}\n" cur_indent (catmap (statement ~indent) e) cur_indent
    | Do_if            (e, a, b) ->
      spr "%sIf [%s] Then\n%s%sElse\n%s\n"
        cur_indent (bool_expression e) (statement ~indent a) 
        cur_indent (statement ~indent b)
    | Do_while_loop    (e, a)    -> 
      spr "%sWhile [%s] Do\n%s\n" 
        cur_indent (bool_expression e) (statement ~indent a)
    | Do_assign_int    (a, b) -> assign (int_variable    a) (int_expression    b)
    | Do_assign_buffer (a, b) -> assign (buffer_variable a) (buffer_expression b)
    | Do_assign_bool   (a, b) -> assign (bool_variable   a) (bool_expression   b)


end



module Construct = struct

  exception Stiel_construction_error of string
  let fail s = raise (Stiel_construction_error ("STIEL.Construct: " ^ s))

  let nop () = Do_nothing

  let block l = Do_block l

  let conditional
      ?(block_then) ?(statement_then)
      ?(block_else) ?(statement_else) condition =
    let actual_block bl st =
      match bl, st with
      | None, None -> Do_nothing
      | Some l, None -> l
      | None, Some s -> Do_block s
      | Some s, Some l -> Do_block (s :: l) in
    Do_if (condition, actual_block block_then statement_then,
           actual_block block_else statement_else)

  let while_loop c s = Do_while_loop (c, s)

  let rec int = function
    | `uint i -> Int_expr_literal (Int64.of_int i) 
    | `u64  i -> Int_expr_literal i 
    | `minus m   -> Int_expr_unary (Int_unary_minus, int m)
    | `plus  m   -> Int_expr_unary (Int_unary_plus , int m)
    | `big2lit m -> Int_expr_unary (Int_unary_big2little_endian, int m)
    | `lit2big m -> Int_expr_unary (Int_unary_little2big_endian, int m)
    | `add     (a, b) -> fail "todo"     
    | `sub     (a, b) -> fail "todo"     
    | `mul     (a, b) -> fail "todo"     
    | `div     (a, b) -> fail "todo"     
    | `modulo  (a, b) -> fail "todo"     
    | `band    (a, b) -> fail "todo"     
    | `bor     (a, b) -> fail "todo"     
    | `bin_xor (a, b) -> fail "todo"   
    | `bin_shl (a, b) -> fail "todo"    
    | `bin_shr (a, b) -> fail "todo"   
    | `var v -> Int_expr_variable v
    | `buffer_content (*integer_type * buffer_expression*)
      -> fail "NOT IMPLEMENTED"


  module Input_language = struct

    let lexer = 
      Genlex.make_lexer
        ["+";"-";"*";"/"; "%"; "("; ")";
         "="; ">"; "<"; "<="; ">=";
         "and"; "or"; "not";
         "band"; "bor"; ">>"; "<<"; ]  

    let expression_of_string str =
      let s = Stream.of_string str in
      let rec construct lexed =
        match Stream.peek lexed with
        | Some (Genlex.Kwd s) -> fail "Not implemented"
        | Some (Genlex.Ident s) -> Int_expr_variable s
        | Some (Genlex.Int o) ->  Int_expr_literal (Int64.of_int o)
        | Some (Genlex.Float o) -> fail "floats not defined"
        | Some (Genlex.String o) -> fail "string literals not defined"
        | Some (Genlex.Char o) -> fail "chars not defined"
        | _ -> fail "default fail"
      in
      construct (lexer s)

  end

end
