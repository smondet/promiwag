
(* Simple Typed Imperative Embedded Language *)

open Promiwag_std

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

type statement =
  | Do_nothing
  | Do_comment of string
  (* | Do_int_evaluation of int_expression ---> expressions must keep
     purely functional hence, evaluation is useless *)
  | Do_block of statement list
  | Do_if of bool_expression * statement * statement
  | Do_while_loop of bool_expression * statement
  | Do_assignment  of variable_name * typed_expression
  | Do_declaration of typed_variable
  | Do_log of string * typed_expression list

module Construct = struct

  exception Stiel_construction_error of string
  let fail s = raise (Stiel_construction_error ("STIEL.Construct: " ^ s))

  let fitted_uint ?(fail=failwith) i =
    if 1 <= i && i <= 8 then (Type_uint8, 8)
    else if   9 <= i && i <= 16 then (Type_uint16, 16)
    else if  17 <= i && i <= 32 then (Type_uint32, 32)
    else if  33 <= i && i <= 64 then (Type_uint64, 64)
    else 
      fail (sprintf "fitted_uint: too long integer: %d" i)


  let rec int = function
    | `U i -> Int_expr_literal (Int64.of_int i) 
    | `U64  i -> Int_expr_literal i 
    | `Minus m   -> Int_expr_unary (Int_unary_minus, int m)
    | `Plus  m   -> Int_expr_unary (Int_unary_plus , int m)

    | `Add     (a, b) -> Int_expr_binary (Int_binop_add    , int a, int b)
    | `LsAdd (h :: t) -> Int_expr_binary (Int_binop_add    , int h, int (`LsAdd t))
    | `LsAdd []       -> Int_expr_literal Int64.zero
    | `Sum l -> int (`LsAdd l)

    | `Sub     (a, b) -> Int_expr_binary (Int_binop_sub    , int a, int b)

    | `Mul     (a, b) -> Int_expr_binary (Int_binop_mul    , int a, int b)
    | `LsMul (h :: t) -> Int_expr_binary (Int_binop_mul    , int h, int (`LsMul t))
    | `LsMul []       -> Int_expr_literal Int64.one
    | `Prod l -> int (`LsMul l)

    | `Div     (a, b) -> Int_expr_binary (Int_binop_div    , int a, int b)
    | `Mod     (a, b) -> Int_expr_binary (Int_binop_mod    , int a, int b)
    | `Band    (a, b) -> Int_expr_binary (Int_binop_bin_and, int a, int b)
    | `Bor     (a, b) -> Int_expr_binary (Int_binop_bin_or , int a, int b)
    | `Bxor (a, b) -> Int_expr_binary (Int_binop_bin_xor, int a, int b)
    | `Bshl (a, b) -> Int_expr_binary (Int_binop_bin_shl, int a, int b)
    | `Bshr (a, b) -> Int_expr_binary (Int_binop_bin_shr, int a, int b)
    | `Var v -> Int_expr_variable v
    |   `U8_at b -> Int_expr_buffer_content (Get_native Type_uint8       ,  b)
    |  `U16_at b -> Int_expr_buffer_content (Get_native Type_uint16      ,  b)
    |  `U32_at b -> Int_expr_buffer_content (Get_native Type_uint32      ,  b)
    |  `U64_at b -> Int_expr_buffer_content (Get_native Type_uint64      ,  b)
    | `Unat_at b -> Int_expr_buffer_content (Get_native Type_uint_native ,  b)
    |   `U8_Big_at b -> Int_expr_buffer_content (Get_big_endian Type_uint8       ,  b)
    |  `U16_Big_at b -> Int_expr_buffer_content (Get_big_endian Type_uint16      ,  b)
    |  `U32_Big_at b -> Int_expr_buffer_content (Get_big_endian Type_uint32      ,  b)
    |  `U64_Big_at b -> Int_expr_buffer_content (Get_big_endian Type_uint64      ,  b)
    | `Unat_Big_at b -> Int_expr_buffer_content (Get_big_endian Type_uint_native ,  b)
    |   `U8_Little_at b -> Int_expr_buffer_content (Get_little_endian Type_uint8       ,  b)
    |  `U16_Little_at b -> Int_expr_buffer_content (Get_little_endian Type_uint16      ,  b)
    |  `U32_Little_at b -> Int_expr_buffer_content (Get_little_endian Type_uint32      ,  b)
    |  `U64_Little_at b -> Int_expr_buffer_content (Get_little_endian Type_uint64      ,  b)
    | `Unat_Little_at b -> Int_expr_buffer_content (Get_little_endian Type_uint_native ,  b)
    | `E e -> e

  let uint i = Int_expr_literal (Int64.of_int i) 
  let u64  i = Int_expr_literal i 
  let minus m = Int_expr_unary (Int_unary_minus, m)
  let sum l = int (`LsAdd (Ls.map (fun e -> `E e) l))
  let add a b = Int_expr_binary (Int_binop_add, a, b)
  let sub a b = Int_expr_binary (Int_binop_sub, a, b)
  let mul a b = Int_expr_binary (Int_binop_mul, a, b)
  let prod l  =  int (`LsMul (Ls.map (fun e -> `E e) l))
  let div a b = Int_expr_binary (Int_binop_div, a, b)
  let (mod) a b = Int_expr_binary (Int_binop_mod, a, b)
  let modulo a b = Int_expr_binary (Int_binop_mod, a, b)
  let bin_and a b = Int_expr_binary (Int_binop_bin_and, a, b)
  let bin_or  a b = Int_expr_binary (Int_binop_bin_or , a, b)
  let bin_xor a b = Int_expr_binary (Int_binop_bin_xor, a, b)
  let bin_shl a b = Int_expr_binary (Int_binop_bin_shl, a, b)
  let bin_shr a b = Int_expr_binary (Int_binop_bin_shr, a, b)
  let int_var v =   Int_expr_variable v
  let get_style s t = match s with
    | `big -> Get_big_endian t
    | `little -> Get_little_endian t
    | `native -> Get_native t
  let u8_at   ?(how=`big) b =
    Int_expr_buffer_content (get_style how Type_uint8       ,  b)
  let u16_at  ?(how=`big) b =
    Int_expr_buffer_content (get_style how Type_uint16      ,  b)
  let u32_at  ?(how=`big) b =
    Int_expr_buffer_content (get_style how Type_uint32      ,  b)
  let u64_at  ?(how=`big) b =
    Int_expr_buffer_content (get_style how Type_uint64      ,  b)
  let unat_at ?(how=`big) b =
    Int_expr_buffer_content (get_style how Type_uint_native ,  b)

  let ufitted_at ~size ~how b =
    match size with
    | 8  ->  u8_at ~how b
    | 16 -> u16_at ~how b
    | 32 -> u32_at ~how b
    | 64 -> u64_at ~how b
    | _ -> fail "ufit_at: size not in {8, 16, 32, 64}"

  let ones size =
    if  Int64.to_float Int64.max_int < (2. ** (float size)) then
      failwith "ones: Too big int64 !"
    else
      u64 (Int64.sub (Int64.shift_left 1L size) 1L)

  let rec buffer = function
    | `Var v -> Buffer_expr_variable v
    | `Offset (b, i) -> Buffer_expr_offset (buffer b, int i)

  let buffer_var v = Buffer_expr_variable v
  let offset b i =  Buffer_expr_offset (b, i)

  let rec bool = function
    | `T | `True  -> Bool_expr_true
    | `F | `False -> Bool_expr_false
    | `Var v -> Bool_expr_variable v
    | `And (a, b) -> Bool_expr_and (bool a, bool b)
    | `Or  (a, b) -> Bool_expr_or  (bool a, bool b)
    | `LsAnd (h :: t) -> Bool_expr_and (bool h, bool (`LsAnd t))
    | `LsAnd []       -> Bool_expr_true
    | `LsOr  (h :: t) -> Bool_expr_or  (bool h, bool (`LsOr t))
    | `LsOr  []     -> Bool_expr_false
    | `Not  a     -> Bool_expr_not (bool a)
    | `Eq (a, b) -> Bool_expr_binary_int (Bool_binop_equals           , int a, int b)
    | `Neq(a, b) -> Bool_expr_binary_int (Bool_binop_notequals        , int a, int b)
    | `Gt (a, b) -> Bool_expr_binary_int (Bool_binop_strictly_greater , int a, int b)
    | `Lt (a, b) -> Bool_expr_binary_int (Bool_binop_strictly_lower   , int a, int b)
    | `Ge (a, b) -> Bool_expr_binary_int (Bool_binop_equal_or_greater , int a, int b)
    | `Le (a, b) -> Bool_expr_binary_int (Bool_binop_equal_or_lower   , int a, int b)
    | `E e -> e

  let t = Bool_expr_true
  let f = Bool_expr_false
  let bool_var v = Bool_expr_variable v
  let ls_and l = bool (`LsAnd (Ls.map (fun e -> `E e) l))
  let ls_or  l = bool (`LsOr  (Ls.map (fun e -> `E e) l))
  let not  a   = Bool_expr_not a
  let eq  a b = Bool_expr_binary_int (Bool_binop_equals           , a, b)
  let neq a b = Bool_expr_binary_int (Bool_binop_notequals        , a, b)
  let gt  a b = Bool_expr_binary_int (Bool_binop_strictly_greater , a, b)
  let lt  a b = Bool_expr_binary_int (Bool_binop_strictly_lower   , a, b)
  let ge  a b = Bool_expr_binary_int (Bool_binop_equal_or_greater , a, b)
  let le  a b = Bool_expr_binary_int (Bool_binop_equal_or_lower   , a, b)


  let expr = function
    |   `U8  e -> Typed_int (Type_uint8      , int e)
    |  `U16  e -> Typed_int (Type_uint16     , int e)
    |  `U32  e -> Typed_int (Type_uint32     , int e)
    |  `U64  e -> Typed_int (Type_uint64     , int e)
    | `Unat  e -> Typed_int (Type_uint_native, int e)
    |   `U8E e -> Typed_int (Type_uint8      ,     e)
    |  `U16E e -> Typed_int (Type_uint16     ,     e)
    |  `U32E e -> Typed_int (Type_uint32     ,     e)
    |  `U64E e -> Typed_int (Type_uint64     ,     e)
    | `UnatE e -> Typed_int (Type_uint_native,     e)
    | `Bool  e -> Typed_bool e
    | `BoolE e -> Typed_bool (bool e)
    | `Sized    (i, e) -> Typed_buffer (Type_sized_buffer i, buffer e)
    | `Pointer      e  -> Typed_buffer (Type_pointer,        buffer e)
    | `SizedE   (i, e) -> Typed_buffer (Type_sized_buffer i,        e)
    | `PointerE     e  -> Typed_buffer (Type_pointer,               e)

  let expr_u8    e = Typed_int (Type_uint8      , e)
  let expr_u16   e = Typed_int (Type_uint16     , e)
  let expr_u32   e = Typed_int (Type_uint32     , e)
  let expr_u64   e = Typed_int (Type_uint64     , e)
  let expr_unat  e = Typed_int (Type_uint_native, e)
  let expr_bool  e = Typed_bool e
  let expr_pointer e = Typed_buffer (Type_pointer, e)


  let int_expr = function
    | Typed_int (_, e) -> e
    | _ -> fail "This typed expression is not an integer one"
  let buffer_expr = function
    | Typed_buffer (_, e) -> e
    | _ -> fail "This typed expression is not a buffer one"

  let typed_variable ?(unique=true) name kind =
    { name = if unique then Unique.name name else name;
      kind = kind}

  let tv = typed_variable

  let var = 
    function
      | `Sized_buffer (v, i) -> tv v $ Kind_buffer (Type_sized_buffer i)
      | `Pointer v           -> tv v $ Kind_buffer Type_pointer    
      | `U8   v              -> tv v $ Kind_int    Type_uint8      
      | `U16  v              -> tv v $ Kind_int    Type_uint16     
      | `U32  v              -> tv v $ Kind_int    Type_uint32     
      | `U64  v              -> tv v $ Kind_int    Type_uint64     
      | `Unat v              -> tv v $ Kind_int    Type_uint_native
      | `Bool v              -> tv v $ Kind_bool   

  let var_sized_buffer ?unique i v = tv ?unique v $ Kind_buffer (Type_sized_buffer i)
  let var_pointer      ?unique   v = tv ?unique v $ Kind_buffer Type_pointer           
  let var_u8           ?unique   v = tv ?unique v $ Kind_int    Type_uint8             
  let var_u16          ?unique   v = tv ?unique v $ Kind_int    Type_uint16            
  let var_u32          ?unique   v = tv ?unique v $ Kind_int    Type_uint32            
  let var_u64          ?unique   v = tv ?unique v $ Kind_int    Type_uint64            
  let var_unat         ?unique   v = tv ?unique v $ Kind_int    Type_uint_native       
  let var_bool         ?unique   v = tv ?unique v $ Kind_bool   

  let expr_var tv =
    match tv.kind with
    | Kind_bool -> Typed_bool (Bool_expr_variable tv.name)
    | Kind_int t -> Typed_int (t, Int_expr_variable tv.name)
    | Kind_buffer t -> Typed_buffer (t, Buffer_expr_variable tv.name)

  let kind_of_expr = function
    | Typed_bool _        -> Kind_bool 
    | Typed_int (t, _)    -> Kind_int t
    | Typed_buffer (t, _) -> Kind_buffer t

  let assign v te = Do_assignment (v.name, te)
  let assignment a b  = Do_assignment (a, b)

  let log f l = Do_log (f, l)

  let var_name t = t.name

  let declare v = Do_declaration v
  let declaration n t = Do_declaration (tv ~unique:false n t)

  let declare_and_assign v te = 
    [declare v; assign v te]

  let nop = Do_nothing

  let block l = Do_block l

  let conditional ?(statement_then=Do_nothing) ?(statement_else=Do_nothing)
      condition =
    Do_if (condition, statement_then, statement_else)

  let while_loop c s = Do_while_loop (c, s)

  let cmt s = Do_comment s


end


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

  let rec statement ?(indent=0) =
    let cur_indent = String.make indent ' ' in
    let cat = String.concat "" in
    let catmap f l = cat (List.map f l) in
    let assign a b = spr "%s%s := %s;\n" cur_indent a b in
    let indent = indent + 2 in
    function 
    | Do_nothing                 -> spr "%sNop;\n" cur_indent
    | Do_comment        s        -> spr "%s(* %s *)\n" cur_indent s
    | Do_block          e        -> 
      spr "%s{\n%s%s}\n" cur_indent (catmap (statement ~indent) e) cur_indent
    | Do_if            (e, a, b) ->
      spr "%sIf [%s] Then\n%s%sElse\n%s\n"
        cur_indent (bool_expression e) (statement ~indent a) 
        cur_indent (statement ~indent b)
    | Do_while_loop    (e, a)    -> 
      spr "%sWhile [%s] Do\n%s\n" 
        cur_indent (bool_expression e) (statement ~indent a)
    | Do_assignment  (a, b) -> assign (variable_name    a) (typed_expression b)
    | Do_declaration t -> 
      spr "%sDeclare %s of type: %s;\n" cur_indent
        (variable_name t.name) (typed_variable_kind t.kind)
    | Do_log (f, l) ->
      spr "%sLog (format: %s) [%s];\n" cur_indent f 
        (Str.concat "; " (Ls.map typed_expression l))


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
    (`expression (`cast (`void, `call (`variable "eprintf", 
                                       (`literal_string !fmt) :: arg_list))))

  let rec block compiler = function
    | Do_block          e        ->
      let decls, stats = _check_and_reorder_block compiler e in
      C_cons.block ~declarations:(Ls.map decls ~f:(declaration compiler))
        ~statements:(Ls.map stats ~f:(statement compiler)) ()
    | _ -> fail compiler "Called 'block' on a non-block statement"
      
  and statement compiler s =
    let assign a b = `assignment (`variable a, b) in
    match s with
    | Do_nothing                 -> `empty
    | Do_comment               s -> `block ([], [`comment s])
    | Do_block  e as b -> `block (block compiler b)
    | Do_if            (e, a, b) ->
      `conditional (bool_expression compiler e,
                    statement compiler a,
                    statement compiler b)
    | Do_while_loop    (e, a)    -> 
      `while_loop (bool_expression compiler e, statement compiler a)
    | Do_assignment (a, b) ->
      assign (variable_name a) (typed_expression compiler b)
    | Do_declaration _ -> 
      fail compiler "Calling 'statement' on a declaration"
    | Do_log (f, l) -> printf_of_log compiler f l
  and declaration compiler = function
    | Do_declaration typed_var ->
      `uninitialized (typed_var.name,
                      typed_variable_kind compiler typed_var.kind)
    | _ -> fail compiler "Calling 'declaration' on a non-declaration"

end

module Transform = struct


  module Partial_evaluation = struct
    type environment = {
      int_variables:  (variable_name, int_expression) Environment.t;
      bool_variables: (variable_name, bool_expression) Environment.t;
      use_purity: bool;
      do_symbolic_equality: bool;
    }

    (*  WARNING:
        do_symbolic_equality and use_purity will absorb some side effects.

        For now side effects are just the divisions/modulos by zero.
    *)


    module Env = Environment

    let environment
        ?(do_symbolic_equality=false) ?(use_purity=false)
        ?int_variables ?bool_variables () =
      {int_variables = 
          (match int_variables with | Some s -> s | None -> Env.empty);
       bool_variables = 
          (match bool_variables with | Some s -> s | None -> Env.empty);
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

    let rec buffer_type environment = function
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
      | Int_expr_variable v as original ->
        begin match Env.find_opt environment.int_variables v with
        | None -> original
        | Some e -> int_expression environment e
        end
      | Int_expr_buffer_content (_, _) as original -> original
      | (Int_expr_literal _) as original -> original

    and buffer_expression environment = function
      | Buffer_expr_variable _ as original -> original
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
      | Bool_expr_variable v as original ->
        begin match Env.find_opt environment.bool_variables v with
        | None -> original
        | Some e -> bool_expression environment e
        end
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
