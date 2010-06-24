
open Promiwag_std

module C_LightAST = struct

  type include_filename = string
  type identifier = string
  type type_name = identifier
  type field_name = identifier
  (* type function_name = identifier type variable_name = identifier *)
  type variable_name = identifier
      
  type c_type = [
  | `void
  | `unsigned_char
  | `signed_char
  | `unsigned_short
  | `signed_short
  | `unsigned_int
  | `signed_int
  | `unsigned_long
  | `signed_long
  | `float
  | `double
  | `long_double
  | `named_type of type_name
  | `array of 
      [`empty | `literal_int of int | `variable of variable_name] list * c_type
  | `structure of (field_name * c_type) list
  | `union of c_type list
  | `pointer of c_type
  ]
(* TODO: function types, enums*)

  type function_signature  =
      c_type * variable_name * (variable_name * c_type) list

  type function_declaration = [
  | `extern of function_signature
  | `local of function_signature
  ]

  type binary_operator = [
  | `bin_add   (*  +  *)
  | `bin_sub   (*  -  *)
  | `bin_mul   (*  *  *)
  | `bin_div   (*  /  *)
  | `bin_mod   (*  %  *)
  | `bin_and   (*  && *) 
  | `bin_or    (*  || *) 
  | `bin_band  (*  &  *)
  | `bin_bor   (*  |  *)
  | `bin_xor   (*  ^  *)
  | `bin_shl   (*  << *) 
  | `bin_shr   (*  >> *) 
  | `bin_eq    (*  == *) 
  | `bin_ne    (*  != *) 
  | `bin_lt    (*  <  *)
  | `bin_gt    (*  >  *)
  | `bin_le    (*  <= *) 
  | `bin_ge    (*  >= *) 
  ]

  type unary_operator = [
  | `unary_minus     (* -  *)
  | `unary_plus      (* +  *)
  | `unary_not       (* !  *)
  | `unary_bnot      (* ~  *)
  | `unary_memof     (* *  *)
  | `unary_addrof    (* &  *) 
  | `unary_preincr   (* ++ *)
  | `unary_predecr   (* -- *)
  | `unary_posincr   (* ++ *)
  | `unary_posdecr   (* -- *)
  ]

  type expression = [
  | `unary of unary_operator * expression
  | `binary of binary_operator * expression * expression
  (* | `ternary of expression Exp Exp Exp *)
  | `cast of c_type * expression
  | `call of expression * expression list
  | `variable of variable_name
  (* | `ESize Exp -- sizeof *)
  | `type_sizeof of c_type
  | `array_index of expression * expression
  | `dot_field of expression * field_name
  | `arrow_field of expression * field_name
  | `literal_int of int
  | `literal_float of float
  | `literal_char of char
  | `literal_string of string
  | `compound_literal of expression list
  ]

  (* Not happy about this: *)
  type case_expression = [
  | `literal_int of int
  | `literal_char of char
  | `identifier of identifier
  | `default
  ]

  type local_variable = [
  | `initialized of variable_name * c_type * expression
  | `uninitialized of variable_name * c_type
  | `comment of string
  ]
  type block = (local_variable list) * (statement list)
  and statement = [
  | `empty
  | `expression of expression
  | `block of block
  | `conditional of expression * statement * statement
  | `while_loop of expression * statement
  | `dowhile_loop of expression * statement
  (* | `for_loop of statement * statement * statement * statement *)
  (*| `break
    | `continue*)
  | `return of expression
  | `return_void
  | `simple_switch of expression * ((case_expression * block) list)
  (* | `label labelname | `goto gototarget *)
  | `assignment of expression * expression
  | `comment of string
  ]
(*
| `assign                        -- = 
| `addassign                     -- += 
| `subassign                     -- -= 
| `MulAssign                     -- *= 
| `DivAssign                     -- /= 
| `ModAssign                     -- %= 
| `BandAssign                    -- &= 
| `BorAssign                     -- |= 
| `XorAssign                     -- ^= 
| `ShlAssign                     -- <<= 
| `ShrAssign                     -- >>= 
| `Comma                         -- ,       
*)

  type function_definition  = 
      function_signature * block

  type toplevel = [
  | `sharp_include of include_filename
  | `comment of string
  | `type_definition of type_name * c_type
  | `function_declaration of function_declaration
  | `function_definition of function_definition
  ]

  type file = toplevel list

end


module type BIG_STRING = sig
  type t
  val str: string -> t
  val cat: t list -> t
  val new_line:  unit -> t
end

module To_big_string(Big_string: BIG_STRING) = struct
    
  module C = C_LightAST
  module BS = Big_string
  let spr = Printf.sprintf

  (* Function composition: *)
  let ( *** ) f g x = f (g x)

  let parentize ?(paren=("(", ")")) thing =
    BS.cat [ BS.str (fst paren); thing; BS.str (snd paren) ]

  let space () = BS.str " "

  let rec sep_list ~str ~map ~sep = function
    | [] -> str ""
    | [one] -> map one
    | one :: more -> 
      BS.cat [map one; (str sep); sep_list ~str ~sep ~map more]  

  let rec type_to_str = function
    | `void -> "void"
    | `unsigned_char -> "unsigned char"
    | `signed_char   -> "char"  
    | `unsigned_short-> "unsigned short"
    | `signed_short  -> "short" 
    | `unsigned_int  -> "unsigned int" 
    | `signed_int    -> "int"   
    | `unsigned_long -> "unsigned long"
    | `signed_long   -> "long"  
    | `float         -> "float"        
    | `double        -> "double"       
    | `long_double   -> "long double"
    | `named_type type_name -> type_name
    | `array _ as t-> typed_var_str ("", t)
    | `structure items -> 
      spr "struct {%s;}" (String.concat "; " (List.map typed_var_str items))
    | `union types -> 
      spr "union {%s}" 
        (String.concat "" (List.map (fun t -> type_to_str t) types))
    | `pointer t -> spr "%s *"  (type_to_str t)
  and typed_var_str: C.variable_name * C.c_type -> string = fun (name, typ) ->
    match typ with
    | `array ([], t) ->
      (spr "%s %s[]" (type_to_str t) name)
    | `array (size_list, t) ->
      let sz =
        String.concat "" (List.map (function
          | `literal_int i -> spr "[%d]" i
          | `variable v -> spr "[%s]" v
          | `empty -> "[]") size_list) in
      (spr "%s %s%s" (type_to_str t) name sz)
    | any_other -> 
      (spr "%s %s" (type_to_str any_other) name)

  let c_type: C.c_type -> BS.t = (BS.str *** type_to_str)

  let typed_var:  C.variable_name * C.c_type -> BS.t = (BS.str *** typed_var_str)

  let function_signature: C.function_signature -> BS.t =
    fun (t, v, args) ->
      BS.cat [
        BS.str (spr "%s %s" (type_to_str t) v);
        parentize (sep_list ~str:BS.str ~sep:", " ~map:typed_var args); ]

  let function_declaration: C.function_declaration -> BS.t = function
    | `extern signature -> 
      BS.cat [(BS.str "extern "); (function_signature signature); BS.str ";\n"]
    | `local signature ->
      BS.cat [(function_signature signature); BS.str ";\n"]

  let binary_operator: C.binary_operator -> BS.t = function
    | `bin_add   -> BS.str "+"
    | `bin_sub   -> BS.str "-"
    | `bin_mul   -> BS.str "*"
    | `bin_div   -> BS.str "/"
    | `bin_mod   -> BS.str "%"
    | `bin_and   -> BS.str "&&" 
    | `bin_or    -> BS.str "||" 
    | `bin_band  -> BS.str "&"
    | `bin_bor   -> BS.str "|"
    | `bin_xor   -> BS.str "^"
    | `bin_shl   -> BS.str "<<" 
    | `bin_shr   -> BS.str ">>" 
    | `bin_eq    -> BS.str "==" 
    | `bin_ne    -> BS.str "!=" 
    | `bin_lt    -> BS.str "<"
    | `bin_gt    -> BS.str ">"
    | `bin_le    -> BS.str "<=" 
    | `bin_ge    -> BS.str ">=" 

  let unary_operator: C.unary_operator -> BS.t = function
    | `unary_minus    -> BS.str "-"
    | `unary_plus     -> BS.str "+"
    | `unary_not      -> BS.str "!"
    | `unary_bnot     -> BS.str "~"
    | `unary_memof    -> BS.str "*"
    | `unary_addrof   -> BS.str "&" 
    | `unary_preincr  -> BS.str "++"
    | `unary_predecr  -> BS.str "--"
    | `unary_posincr  -> BS.str "++"
    | `unary_posdecr  -> BS.str "--"

  let rec expression: C.expression -> BS.t = function
    | `unary (op, exp) ->
      BS.cat [ unary_operator op; parentize (expression exp) ]
    | `binary (bop, expa, expb) -> 
      BS.cat [ parentize (expression expa);
               space (); binary_operator bop; space ();
               parentize (expression expb); ]
    | `cast (typ, exp) -> 
      BS.cat [ parentize (c_type typ); parentize (expression exp)]
    | `call (funct, arglist) ->
      let expr_func = 
        match funct with (* Small de-parentization optimisation. *)
        | `variable v -> BS.str v
        |  _ -> parentize (expression funct) in
      BS.cat [expr_func; 
              parentize (sep_list ~str:BS.str
                           ~map:expression ~sep:", " arglist); ]
    | `variable name -> BS.str name
    | `type_sizeof t -> 
      BS.cat [ BS.str "sizeof(" ; c_type t; BS.str ")"; ]
    | `array_index (expa, expb) -> 
      BS.cat [ parentize (expression expa); 
               parentize ~paren:("[", "]") (expression expb); ]
    | `dot_field (exp, field_name) -> 
      BS.cat [ parentize (expression exp); BS.str "."; BS.str field_name; ]
    | `arrow_field (exp, field_name) -> 
      BS.cat [ parentize (expression exp); BS.str "->"; BS.str field_name; ]
    | `literal_int i -> BS.str (spr "%d" i)
    | `literal_float f -> BS.str (string_of_float f)
    | `literal_char c -> parentize ~paren:("'", "'") (BS.str (String.make 1 c))
    | `literal_string s -> 
      parentize ~paren:("\"", "\"") (BS.str (String.escaped s))
    | `compound_literal exps -> 
      parentize ~paren:("{", "}") 
        (sep_list ~str:BS.str ~map:expression ~sep:", "  exps)

  let  case_expression: C.case_expression -> BS.t = 
    let case = parentize ~paren:("case ", ":") in
    function
      | `literal_int i -> case (BS.str (string_of_int i))
      | `literal_char c ->
        case (parentize ~paren:("'", "'") (BS.str (String.make 1 c)))
      | `identifier identifier -> case (BS.str identifier)
      | `default -> BS.str "default:"

  let comment s =
    let sanitize_comment =
      (* nothing: for now escape-from-comment can be used for lowlevel
         dirty hacking. *)
      fun s -> s in
    BS.cat [ BS.new_line (); BS.str "/* "; BS.str (sanitize_comment s);
             BS.str " */"; BS.new_line () ]


  let local_variable: C.local_variable -> BS.t = function
    | `initialized (name, typ, exp) -> 
      BS.cat [typed_var (name, typ); BS.str " = "; expression exp; BS.str ";"]
    | `uninitialized (name, typ) ->
      BS.cat [typed_var (name, typ); BS.str ";"]
    | `comment s -> comment s

  let and_newline printed = BS.cat [printed; BS.new_line ()]


  let rec block: C.block -> BS.t = fun (local_vars, statements) ->
    BS.cat [
      BS.str " {"; BS.new_line ();
      BS.cat (List.map (and_newline *** local_variable) local_vars);
      BS.cat (List.map (and_newline *** statement) statements);
      BS.str "}"; BS.new_line (); ]
  and statement: C.statement -> BS.t = function
    | `empty -> BS.str ";"
    | `expression exp -> BS.cat [expression exp; BS.str ";"]
    | `block b -> block b; 
    | `conditional (exp, thn, els) ->
      BS.cat [ BS.str "if "; parentize (expression exp); BS.new_line ();
               statement thn; BS.str "else "; statement els; ]
    | `while_loop (exp, st) ->
      BS.cat [ BS.str "while "; parentize (expression exp); statement st;]
    | `dowhile_loop (exp, st) ->
      BS.cat [ BS.str "do "; statement st;
               BS.str "while "; parentize (expression exp); BS.str ";"; ]
    | `return exp ->
      BS.cat [BS.str "return "; expression exp; BS.str ";"]
    | `return_void -> BS.str "return;"
    | `simple_switch (exp, statements) ->
      BS.cat [
        (BS.str "switch "); (parentize (expression exp)); (BS.str "{");
        (BS.new_line ());
        BS.cat (List.map 
                  (fun (casexp, blok) -> 
                    BS.cat [
                      case_expression casexp; 
                      block blok; BS.str "break;"; BS.new_line ()])
                  statements);
        BS.str "}"; ]
    | `assignment (var, exp) -> 
      BS.cat [expression var; BS.str " = "; expression exp; BS.str ";"; ]
    | `comment s -> comment s


  let function_definition: C.function_definition -> BS.t = 
    fun (sign, blk) ->
      BS.cat [function_signature sign; block blk]

  let toplevel: C.toplevel -> BS.t = function
    | `sharp_include file ->
      BS.cat [BS.str "#include <"; BS.str file; BS.str ">"; BS.new_line ();]
    | `type_definition (name, def) ->
      BS.cat [BS.str "typedef "; typed_var (name, def);
              BS.str ";"; BS.new_line ();]
    | `function_declaration f ->
      BS.cat [function_declaration f; BS.new_line ();]
    | `function_definition f ->
      BS.cat [function_definition f; BS.new_line ();]
    | `comment s -> comment s
      

  let file: C.file -> BS.t = fun l -> BS.cat (List.map toplevel l)
    
end

module Unique = struct

  let var_count = ref 0

  let name n = 
    incr var_count;
    Printf.sprintf "%s_%d" n !var_count

end
  
module Typed_expression = struct

  type t = {
    expression: C_LightAST.expression;
    c_type: C_LightAST.c_type;
  }
  let create ?(expression = `literal_int 0) ?(c_type = `signed_int) () = 
    { expression = expression; c_type = c_type; }

  let expression t = t.expression
  let c_type t = t.c_type

  let check_typing: t -> bool = fun _ ->
    failwith "Typed_expression.check_typing: Yes, maybe one day ..."

end


module Variable = struct
    
  type t = {
    name: string;
    c_type: C_LightAST.c_type;
    initialisation: C_LightAST.expression option;
  }

  let create ?(unique=true) ?(name="unamed_")
      ?(c_type=`pointer `void) ?initialisation () =
    let actual_name = if unique then Unique.name name else name in
    {name = actual_name; c_type = c_type; initialisation = initialisation;}

  let declaration t = 
    match t.initialisation with
    | None -> `uninitialized (t.name, t.c_type)
    | Some i -> `initialized (t.name, t.c_type, i)

  let expression t = (`variable t.name : C_LightAST.expression)

  let assignment ?(cast=false) t expr =
    let e = if cast then `cast (t.c_type, expr) else expr in
    (`assignment (expression t, e): C_LightAST.statement)

  let typed_expression t =
    Typed_expression.create ~expression:(expression t) ~c_type:t.c_type ()

  let address_typed_expression t =
    Typed_expression.create 
      ~expression:(`unary  (`unary_addrof, expression t))
      ~c_type:(`pointer t.c_type) ()


  let function_argument t = (t.name, t.c_type)

end

module Function = struct

  type t = {
    name: string;
    return_type: C_LightAST.c_type;
    arguments:  Variable.t list;
    block: C_LightAST.block;
  }

  let create ?(unique=true) ?(name="unamed")
      ?(return_type=`void) ?(arguments=[]) ?(block=([],[])) () =
    let actual_name = if unique then Unique.name name else name in
    {name = actual_name; return_type = return_type;
     arguments = arguments; block = block;}
            
  let signature: t -> C_LightAST.function_signature = fun t -> 
    (t.return_type, t.name, List.map Variable.function_argument t.arguments)

  let local_declaration: t -> C_LightAST.toplevel = fun t ->
    (`function_declaration (`local (signature t)))
        
  let definition: t -> C_LightAST.toplevel = fun t ->
    (`function_definition (signature t, t.block))

  let variable t = `variable t.name

end


module Construct = struct

  let void_named_call s args =
    (`expression (`cast (`void, `call (`variable s, args))) : C_LightAST.statement)

  let assignment_call v f args =
    Variable.assignment v (`call (`variable f, args))

  let sharp_includes s_list = 
    (List.map (fun s -> `sharp_include s) s_list : C_LightAST.toplevel list)

  let standard_main block =
    let argc =
      Variable.create ~name:"argc" ~unique:false ~c_type:`signed_int () in
    let argv =
      Variable.create ~name:"argv" ~unique:false
        ~c_type:(`pointer (`pointer `signed_char)) () in
    let main =
      Function.create ~return_type:`signed_int ~unique:false
        ~name:"main" ~arguments:[argc; argv] ~block () in
    (main, argc, argv)

  let block ?(declarations=[]) ?(statements=[]) () =
    ((declarations, statements) : C_LightAST.block)
   
  let ones_int_literal size =
    if  float max_int < (2. ** (float size)) then
      failwith "ones_int_literal: Too big int"
    else
      `literal_int ((1 lsl size) - 1)

   
end
