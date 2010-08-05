
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
    | Logic_variability of bool_expression * bool_expression

  type statement_annotation = 
    | Annot_comment of string
    | Annot_specify of logic
    | Annot_alternative of [ `C | `Why ] * string


  type statement =
    | Do_nothing
   (* | Do_comment of string *)
  (* | Do_int_evaluation of int_expression ---> expressions must keep
     purely functional hence, evaluation is useless *)
    | Do_block of statement list
    | Do_if of bool_expression * statement * statement
    | Do_while_loop of bool_expression * statement
    | Do_exit_while
    | Do_assignment  of variable_name * typed_expression
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
      before (bool_expression i) before (bool_expression v)

  let statement_annotation ?(before="") = function
    | Annot_comment s -> spr "%s%s" before s
    | Annot_specify l -> logic ~before l
    | Annot_alternative (`C, s) -> 
      spr "%sC: %s" before s
    | Annot_alternative (`Why, s) -> 
      spr "%sWhy: %s" before s


  let rec statement ?(indent=0) =
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
    | Do_while_loop    (e, a)    -> 
      spr "%sWhile [%s] Do\n%s\n" 
        cur_indent (bool_expression e) (statement ~indent a)
    | Do_exit_while -> spr "%sExit While;\n" cur_indent
    | Do_assignment  (a, b) -> assign (variable_name    a) (typed_expression b)
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


module To_format = struct
  open Easy_format

  type compiler = {
    atom_type: atom_param;
    atom_keyword: atom_param;
    atom_operator: atom_param;
    atom_funciton: atom_param;
    atom_literal: atom_param;
    atom_comment: atom_param;
    list_expression:  string * string * string * list_param;
    list_statement:  string * string * string * list_param;
    list_sentence:  string * string * string * list_param;
    list_coma:  string * string * string * list_param;
    list_typed_expression: string * string * string * list_param;
    list_block: string * string * string * list_param;
    list_statements: string * string * string * list_param;
    list_variability: string * string * string * list_param;
    list_comment: string * string * string * list_param;
    label_statement: label_param;
    label_logic: label_param;
    label_alternative: label_param;
  }
  let default_compiler () =
    let list_for_statements = 
      { list with
        stick_to_label = true;
        align_closing = true;
        wrap_body = `Force_breaks;
        indent_body = 0;
      } in
    let list_for_sentence =
      { list with
        stick_to_label = true;
        align_closing = false;
        space_after_opening = false;
        space_before_closing = false;
        wrap_body = `Always_wrap;
        indent_body = 0;
      } in
    let label = {label with indent_after_label = 4 } in
    {
      atom_type =                 atom ;  
      atom_keyword =              atom ;     
      atom_operator =             atom ;      
      atom_funciton =             atom ;      
      atom_literal =              atom ;     
      atom_comment =              atom ;     
      list_expression = ("(", "", ")", list_for_sentence);
      list_statement = ("", "", ";", list_for_sentence);
      list_sentence = ("", "", "", list_for_sentence);
      list_coma = ("(", ",", ")", list_for_sentence);
      list_typed_expression = ("[", ":", "]", list_for_sentence);              
      list_block =  ("{", "", "}", {list_for_statements with indent_body = 4;} );
      list_statements =  ("", "", "", list_for_statements);
      list_variability =  ("", "", "", list);
      list_comment =  ("(*", "", "*)", list);
      label_statement =               label;    
      label_logic =               label;    
      label_alternative =         label;          
    }

  let integer_type compiler t =
    Atom (To_string.integer_type t, compiler.atom_type)

  let int_unary_operator compiler o =
    Atom (To_string.int_unary_operator o, compiler.atom_operator)

  let int_binary_operator compiler o =
    Atom (To_string.int_binary_operator o, compiler.atom_operator)

  let bool_binary_operator compiler o =
    Atom (To_string.bool_binary_operator o, compiler.atom_operator)

  let get_int_at_buffer compiler o =
    Atom (To_string.get_int_at_buffer o, compiler.atom_funciton)

  let variable_name compiler o =
    Atom (To_string.variable_name o, compiler.atom_literal)

  let rec buffer_type compiler o =
    Atom (To_string.buffer_type o, compiler.atom_type)

  and int_expression compiler = function
    | Int_expr_unary  (op, ex)        ->
      List (compiler.list_expression,
            [ int_unary_operator compiler op; int_expression compiler ex])
    | Int_expr_binary (op, ea, eb)    -> 
      List (compiler.list_expression,
        [(int_expression compiler ea);
         (int_binary_operator compiler op);
         (int_expression compiler eb)])
    | Int_expr_variable  v            -> variable_name compiler v
    | Int_expr_buffer_content (t, ex) ->
      List (compiler.list_expression,
            [get_int_at_buffer compiler t;
             Atom ("@", compiler.atom_operator);
             buffer_expression compiler ex])
    | Int_expr_literal        i64     ->
      Atom (Int64.to_string i64, compiler.atom_literal)

  and buffer_expression compiler = function
    | Buffer_expr_variable v         -> variable_name compiler v
    | Buffer_expr_offset (bex, iex)  -> 
      List (compiler.list_expression,
            [buffer_expression compiler bex;
             Atom (":>", compiler.atom_operator);
             int_expression compiler iex])

  and bool_expression compiler = function
    | Bool_expr_true   -> Atom ("True", compiler.atom_literal)
    | Bool_expr_false  -> Atom ("False", compiler.atom_literal)
    | Bool_expr_variable v -> variable_name compiler v
    | Bool_expr_and        (a, b)     ->
      List (compiler.list_expression,
            [bool_expression compiler a;
             Atom ("And", compiler.atom_operator);
             bool_expression compiler b])
    | Bool_expr_or         (a, b)     ->
      List (compiler.list_expression,
            [bool_expression compiler a;
             Atom ("Or", compiler.atom_operator);
             bool_expression compiler b])
    | Bool_expr_not        ex         ->
      List (compiler.list_expression,
            [Atom ("Not", compiler.atom_operator);
             bool_expression compiler ex])
    | Bool_expr_binary_int (op, a, b) ->
      List (compiler.list_expression,
            [(int_expression compiler a);
             (bool_binary_operator compiler op);
             (int_expression compiler b)])

  let typed_expression compiler = function
    | Typed_int    (t, e) -> List (compiler.list_typed_expression,
                                   [ integer_type compiler t;
                                     int_expression compiler e])
    | Typed_bool       e  -> List (compiler.list_typed_expression, 
                                   [ Atom ("bool", compiler.atom_type);
                                     bool_expression compiler e])
    | Typed_buffer (t, e) -> List (compiler.list_typed_expression, 
                                   [buffer_type compiler t;
                                    buffer_expression compiler e])

  let typed_variable_kind compiler = function
    | Kind_int     t -> integer_type compiler t
    | Kind_bool      -> Atom ("bool", compiler.atom_type)
    | Kind_buffer  t -> buffer_type compiler t

  let kwd compiler s = Atom (s, compiler.atom_keyword)

  let logic compiler = function
    | Logic_bool_expr b -> 
      Label ((kwd compiler "Logic:", compiler.label_logic), bool_expression compiler b)
    | Logic_variability (i,v) ->
      List (compiler.list_variability,
            [
              Label ((kwd compiler "Invariant:", compiler.label_logic),
                     bool_expression compiler i);
              Label ((kwd compiler "Variant:", compiler.label_logic),
                     bool_expression compiler v);])

  let statement_annotation compiler = function
    | Annot_comment s -> List (compiler.list_comment, [Atom (s, compiler.atom_comment)])
    | Annot_specify l ->  List (compiler.list_comment, [logic compiler l])
    | Annot_alternative (`C, s) -> 
      List (compiler.list_comment, 
            [Atom (sprintf "C: %s" s, compiler.atom_comment)])
    | Annot_alternative (`Why, s) -> 
      List (compiler.list_comment, 
            [Atom (sprintf "Why: %s" s, compiler.atom_comment)])


  let rec statement compiler =
    let stmt l = List (compiler.list_statement, l) in
    let labeled_stmt lb l = Label ((lb, compiler.label_statement), stmt l) in
    function 
    | Do_nothing                 -> stmt [kwd compiler "Nop"]
    | Do_block          e        -> 
      List (compiler.list_block, Ls.map (statement compiler) e)
    | Do_if            (e, a, b) ->
      List (compiler.list_statements,
            [ Label ((kwd compiler "If", compiler.label_alternative),
                     bool_expression compiler e);
              Label ((kwd compiler "Then", compiler.label_alternative),
                     statement compiler a);
              Label ((kwd compiler "Else", compiler.label_alternative),
                     statement compiler b);])
    | Do_while_loop    (e, a)    -> 
      Label (
        (List (compiler.list_sentence,
               [ kwd compiler "While";
                 bool_expression compiler e;
                 kwd compiler "Do" ]), compiler.label_alternative),
        statement compiler a)
    | Do_exit_while -> stmt [kwd compiler "Exit While"]
    | Do_assignment  (a, b) -> 
      labeled_stmt (variable_name compiler a) [
        Atom (":=", compiler.atom_operator);
        typed_expression compiler b;
      ]
    | Do_declaration t -> 
      labeled_stmt (kwd compiler "Declare") [
        variable_name compiler t.name;
        kwd compiler "As";
        typed_variable_kind compiler t.kind;
      ]
    | Do_log (f, l) ->
      labeled_stmt (kwd compiler "Log") [
        Atom (sprintf "(Format: %S)" f, compiler.atom_literal);
        List (compiler.list_coma,
              Ls.map (typed_expression compiler) l);
      ]
    | Do_annotated_statement (annot, st) ->
      List (compiler.list_statements,
            [ statement_annotation compiler annot;
              statement compiler st ])


end

module With_formatter = struct

  let statement_to_string s =
    (Easy_format.Pretty.to_string
       (To_format.statement (To_format.default_compiler ()) s))

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
      
  let while_loop c s = Do_while_loop (Expression.bool c, s)
  let exit_while = Do_exit_while

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



module To_C = struct

  type compiler = {
    platform: Promiwag_platform.platform;
  }
  let compiler ~platform =
    { platform = platform; }
      
  module Platform = Promiwag_platform
  module P = Promiwag_platform.C
  module C = Promiwag_c_backend.C_LightAST
  module C_cons = Promiwag_c_backend.Construct

  let todo s = failwith (sprintf "Promiwag_stiel.To_C: %s: NOT IMPLEMENTED" s)
  let fail compiler s =
    failwith (sprintf "Promiwag_stiel.To_C: ERROR: %s" s)
  let spr = Printf.sprintf

  let  integer_type compiler = function
    | Type_uint8       -> P.uint8       compiler.platform
    | Type_uint16      -> P.uint16      compiler.platform  
    | Type_uint32      -> P.uint32      compiler.platform
    | Type_uint64      -> P.uint64      compiler.platform
    | Type_uint_native -> P.uint_native compiler.platform

  let size_of_integer_type compiler = function
    | Type_uint8       -> 8 
    | Type_uint16      -> 16
    | Type_uint32      -> 32
    | Type_uint64      -> 64
    | Type_uint_native -> P.size_of_native_uint compiler.platform

  let int_unary_operator compiler = function
    | Int_unary_minus             -> `unary_minus
    | Int_unary_plus              -> `unary_plus

  let int_binary_operator compiler = function
    | Int_binop_add     -> `bin_add
    | Int_binop_sub     -> `bin_sub
    | Int_binop_mul     -> `bin_mul
    | Int_binop_div     -> `bin_div
    | Int_binop_mod     -> `bin_mod
    | Int_binop_bin_and -> `bin_band
    | Int_binop_bin_or  -> `bin_bor 
    | Int_binop_bin_xor -> `bin_xor 
    | Int_binop_bin_shl -> `bin_shl 
    | Int_binop_bin_shr -> `bin_shr 
      
  let bool_binary_operator compiler = function
    | Bool_binop_equals            -> `bin_eq
    | Bool_binop_notequals         -> `bin_ne
    | Bool_binop_strictly_greater  -> `bin_gt
    | Bool_binop_strictly_lower    -> `bin_lt
    | Bool_binop_equal_or_greater  -> `bin_ge
    | Bool_binop_equal_or_lower    -> `bin_le

  let get_int_at_buffer compiler =
    let id = fun _ e -> e in
    let big_endianise int_typ e =
      if Platform.endianism compiler.platform <> `big then
        match size_of_integer_type compiler int_typ with
        | 8  -> e
        | 16 -> `call (`variable "ntohs", [e])
        | 32 -> `call (`variable "ntohl", [e])
        | 64 -> todo "Big endian 64 bit integers"
        | _ -> fail compiler "c_type_size not in {8, 16, 32, 64}"
      else
        e
    in
    let get_int endianise it e =
      let c_type = integer_type compiler it in
      let value_at_pointer =
        `unary (`unary_memof, `cast (`pointer c_type, e)) in
      `cast (c_type, endianise it value_at_pointer) in
    function
      | Get_native         it -> get_int id it
      | Get_big_endian     it -> get_int big_endianise it
      | Get_little_endian  it -> todo "Get_little_endian"

  let variable_name s = s

  let rec buffer_type compiler = function
    | Type_sized_buffer   i -> `array ([`literal_int i], `unsigned_char)
    | Type_pointer          -> `pointer `unsigned_char
    | Type_sizable_buffer s -> todo "Type_sizable_buffer"

  and int_expression compiler = function
    | Int_expr_unary  (op, ex)        ->
      `unary (int_unary_operator compiler op, int_expression compiler ex)
    | Int_expr_binary (op, ea, eb)    -> 
      `binary (int_binary_operator compiler op,
               int_expression compiler ea, int_expression compiler eb)
    | Int_expr_variable  v            -> `variable (variable_name v)
    | Int_expr_buffer_content (t, ex) ->
      get_int_at_buffer compiler t (buffer_expression compiler ex)
    | Int_expr_literal        i64     -> `literal_int64 i64

  and buffer_expression compiler = function
    | Buffer_expr_variable v         -> `variable (variable_name v)
    | Buffer_expr_offset (bex, iex)  -> 
      `binary (`bin_add, buffer_expression compiler bex,
               int_expression compiler iex)

  and bool_expression compiler = function
    | Bool_expr_true   -> `literal_int 1
    | Bool_expr_false  -> `literal_int 0
    | Bool_expr_variable v -> `variable (variable_name v)
    | Bool_expr_and        (a, b)     ->
      `binary (`bin_and, bool_expression compiler a,
               bool_expression compiler b)
    | Bool_expr_or         (a, b)     ->
      `binary (`bin_or, bool_expression compiler a, 
               bool_expression compiler b)
    | Bool_expr_not        ex         ->
      `unary (`unary_not, bool_expression compiler ex)
    | Bool_expr_binary_int (op, a, b) ->
      `binary (bool_binary_operator compiler op,
               int_expression compiler a,
               int_expression compiler b)
        
  let _check_and_reorder_block compiler stlist =
    let decls = ref [] in
    let vars = ref [] in
    let add d v = decls := d :: !decls; vars := v :: !vars; false in
    let non_decls =
      Ls.filter stlist ~f:(function
        | Do_declaration typed_var as d -> add d typed_var.name
        | Do_annotated_statement (ann, Do_declaration tv) as d ->
          add d tv.name
        | _ -> true) in
    let nb_vars = Ls.length !vars in
    let nb_unique_vars = Ls.length (Ls.unique !vars) in
    if nb_vars = nb_unique_vars then
      ((Ls.rev !decls), non_decls)
    else
      fail compiler "There are two variables with the same name \
                     in this block."

  let typed_expression compiler = function 
    | Typed_int    (t, e) -> `cast (integer_type compiler t,
                                    int_expression compiler e)
    | Typed_bool       e  -> `cast (`signed_int, bool_expression compiler e)
    | Typed_buffer (t, e) -> `cast (buffer_type compiler Type_pointer, 
                                    buffer_expression compiler e)
      
  let typed_variable_kind compiler = function
    | Kind_int    t -> integer_type compiler t
    | Kind_bool     -> `signed_int
    | Kind_buffer t -> buffer_type compiler t

  let printf_of_log compiler format te_list =
    let escape = "AROBASEESCAPE4732992387737238292933" in
    let fmt = ref (Str.multi_replace ~str:format ~sub:"@@" ~by:escape) in
    let arg_list =
      Ls.map te_list ~f:(fun te ->
        let type_fmt lfmt =
          let ux =
            match te with
            | Typed_int (Type_uint8      , _) -> ("%hhu", "%02hhx")
            | Typed_int (Type_uint16     , _) -> ("%hu" , "%04hx")
            | Typed_int (Type_uint32     , _) -> ("%u"  , "%08x")
            | Typed_int (Type_uint64     , _) -> ("%llu", "%16llx")
            | Typed_int (Type_uint_native, _) -> ("%lu" , "%16lx")
            | Typed_bool       _  -> ("%d", "%x")
            | Typed_buffer (t, _) ->  ("%lu", "%lx") in
          match lfmt with
          | "@int" -> fst ux
          | "@hex" -> snd ux 
          | "@expr" -> "%s"
          | _ -> fail compiler "Do_log: should not be here (type_fmt)" in
        let try_the_patterns =
          Ls.map  ["@int"; "@hex"; "@expr" ]  ~f:(fun pat ->
            let index = try Str.find !fmt pat with e -> max_int in
            (index, pat)) in
        let _, the_first =
          Ls.fold_left ~f:(fun (cur_index, cur_pat) (index, pat) ->
            if cur_index > index then (index, pat)
            else (cur_index, cur_pat))
            ~init:(max_int, "none") try_the_patterns in
        let sub, by = the_first, (type_fmt the_first) in
        match the_first with
        | "none" -> fail compiler "Do_log: format does not match arg list"
        | "@int" | "@hex" -> 
          fmt := snd (Str.replace ~str:!fmt ~sub ~by);
          typed_expression compiler te
        | "@expr" ->
          fmt := snd (Str.replace ~str:!fmt ~sub ~by);
          `literal_string (To_string.typed_expression te)
        |  s -> 
          fail compiler (sprintf "Do_log: should not be here (arg_list): %s" s)
      ) in
    fmt := (Str.multi_replace ~str:!fmt ~sub:escape ~by:"@");
    (`expression (`cast (`void, `call (`variable "printf", 
                                       (`literal_string !fmt) :: arg_list))))

  let logic = To_string.logic 

  let statement_annotation = function
    | Annot_comment s -> `comment s
    | Annot_specify l -> `comment (spr "Logic: %s" (logic l))
    | Annot_alternative (`C, s) -> todo "Alternative C code"
    | Annot_alternative (`Why, s) -> 
      `comment (spr "Why code: %s" s)


  let rec block compiler = function
    | Do_block          e        ->
      let decls, stats = _check_and_reorder_block compiler e in
      C_cons.block ~declarations:(Ls.map decls ~f:(declaration compiler))
        ~statements:(Ls.map stats ~f:(statement compiler)) ()
    | _ -> fail compiler "Called 'block' on a non-block statement"
      
  and statement ?(wrong_place_for_comment=false) compiler s =
    let assign a b = `assignment (`variable a, b) in
    match s with
    | Do_nothing                 -> `empty
    | Do_block  e as b -> `block (block compiler b)
    | Do_if            (e, a, b) ->
      `conditional (bool_expression compiler e,
                    statement ~wrong_place_for_comment:true compiler a,
                    statement ~wrong_place_for_comment:true compiler b)
    | Do_while_loop    (e, a)    -> 
      `while_loop (bool_expression compiler e, 
                   statement ~wrong_place_for_comment:true compiler a)
    | Do_exit_while ->
      `break
    | Do_assignment (a, b) ->
      assign (variable_name a) (typed_expression compiler b)
    | Do_declaration _ -> 
      fail compiler "Calling 'statement' on a declaration"
    | Do_log (f, l) -> printf_of_log compiler f l
    | Do_annotated_statement (ann, Do_declaration tv) ->
      fail compiler "Calling 'statement' on a declaration"
    | Do_annotated_statement (ann, st) ->
      `list [statement_annotation ann; statement compiler st]
  and declaration compiler = function
    | Do_annotated_statement (ann, Do_declaration tv) ->
      `list [statement_annotation ann; 
             declaration compiler (Do_declaration tv)]
    | Do_declaration typed_var ->
      `uninitialized (typed_var.name,
                      typed_variable_kind compiler typed_var.kind)
    | _ -> fail compiler "Calling 'declaration' on a non-declaration"

  let cabled_list_of_statements compiler var_expr_list statements =
    let module CTE = Promiwag_c_backend.Typed_expression in
    let declarations, assignments =
      let decls = ref [] in
      let assis = ref [] in
      Ls.iter var_expr_list ~f:(fun (stiel_var, c_typed_expr) ->
        decls := declaration compiler (Variable.declare stiel_var) :: !decls;
        assis := (
          let c_type = 
            typed_variable_kind compiler (Variable.kind stiel_var) in
          `assignment (`variable (Variable.name stiel_var),
                       `cast (c_type, CTE.expression c_typed_expr)))
        :: !assis;
      );
      (Ls.rev !decls, Ls.rev !assis) in
    let decls, stats = _check_and_reorder_block compiler statements in
    C_cons.block
      ~declarations:(declarations @ (Ls.map decls ~f:(declaration compiler)))
        ~statements:(assignments @ (Ls.map stats ~f:(statement compiler))) ()

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
  }
  let compiler ?(on_variables=fun _ -> ()) () =
    { on_variables = on_variables }

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
    | Do_while_loop    (e, a)    -> 
      ignore (bool_expression compiler e, statement compiler a)
    | Do_exit_while -> ()
    | Do_assignment  (a, b) -> 
      ignore (variable_name compiler a, typed_expression compiler b)
    | Do_declaration t -> 
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
  module Stiel_to_str = To_string
  module Stiel_to_C = To_C
end




