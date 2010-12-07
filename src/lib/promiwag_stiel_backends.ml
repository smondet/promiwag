open Promiwag_std
open Promiwag_stiel
open Definition

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
                     int_expression compiler v);])

  let rec statement_annotation compiler = function
    | Annot_comment s -> List (compiler.list_comment, [Atom (s, compiler.atom_comment)])
    | Annot_specify l ->  List (compiler.list_comment, [logic compiler l])
    | Annot_alternative (`C, s) -> 
      List (compiler.list_comment, 
            [Atom ("C:", compiler.atom_comment);
             statement compiler s])
    | Annot_alternative (`Why, s) -> 
      List (compiler.list_comment, 
            [Atom ("Why:", compiler.atom_comment);
             statement compiler s])
  and statement compiler =
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
    | Do_while_loop    (s, e, a)    -> 
      Label (
        (List (compiler.list_sentence,
               [ kwd compiler (sprintf "While:\"%s\"" s);
                 bool_expression compiler e;
                 kwd compiler "Do" ]), compiler.label_alternative),
        statement compiler a)
    | Do_exit_while s -> stmt [kwd compiler (sprintf "Exit While \"%s\"" s)]
    | Do_assignment  (a, b) -> 
      labeled_stmt (variable_name compiler a) [
        Atom (":=", compiler.atom_operator);
        typed_expression compiler b;
      ]
    | Do_external_assignment  (a, f, args) -> 
      labeled_stmt (variable_name compiler a) [
        Atom (":= external:", compiler.atom_operator);
        Atom (f, compiler.atom_operator);
        List (compiler.list_coma, Ls.map (typed_expression compiler) args);
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

module To_C = struct

  type compiler = {
    platform: Promiwag_platform.platform;
    while_stack: string Stack.t;
  }
  let compiler ~platform =
    { platform = platform; while_stack = Stack.create () }
      
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
      `comment (spr "Why code: %s" (To_string.statement s))


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
    | Do_block  e as b -> `block (block compiler b)
    | Do_if            (e, a, b) ->
      `conditional (bool_expression compiler e,
                    statement compiler a, statement compiler b)
    | Do_while_loop    (s, e, a)    ->
      Stack.push s compiler.while_stack;
      let wl =
        `while_loop (bool_expression compiler e, statement compiler a)
      in
      ignore (Stack.pop compiler.while_stack);
      wl
    | Do_exit_while s ->
      begin try 
              let while_s = Stack.top compiler.while_stack in
              if while_s = s then
                `break
              else
                fail compiler (sprintf "Cannot compile 'Exit_while \"%s\";' \
                                     while in 'While:\"%s\"'" s while_s)
        with Stack.Empty ->
          fail compiler (sprintf "Cannot compile 'Exit_while \"%s\";' \
                                 while not in a 'While'" s)
      end
    | Do_assignment (a, b) ->
      assign (variable_name a) (typed_expression compiler b)
    | Do_external_assignment (a, f, args) ->
      assign (variable_name a) 
        (`call (`variable f, Ls.map (typed_expression compiler) args))
    | Do_declaration _ -> 
      fail compiler "Calling 'statement' on a declaration"
    | Do_log (f, l) -> printf_of_log compiler f l
    | Do_annotated_statement (ann, Do_declaration tv) ->
      fail compiler "Calling 'statement' on a declaration"
    | Do_annotated_statement (ann, st) ->
      `block ([], [statement_annotation ann; statement compiler st])
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

module To_why_string = struct


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
    list_specification: string * string * string * list_param;
    label_statement: label_param;
    label_logic: label_param;
    label_alternative: label_param;

    mutable while_list: string list;
  }
  let default_compiler () =
    let list_for_statements = 
      { list with
        stick_to_label = true;
        align_closing = true;
        wrap_body = `Force_breaks;
        separators_stick_left = true;
        space_before_separator = false;
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
      list_statement = ("", "", "", list_for_sentence);
      list_sentence = ("", "", "", list_for_sentence);
      list_coma = ("(", ",", ")", list_for_sentence);
      list_typed_expression = ("[", ":", "]", list_for_sentence);              
      list_block =  ("begin", ";", "end", {list_for_statements with indent_body = 2;} );
      list_statements =  ("", "", "", list_for_statements);
      list_variability =  ("", "", "", list);
      list_comment =  ("(*", "", "*)", list);
      list_specification =  ("{", "", "}", list);
      label_statement =               label;    
      label_logic =               label;    
      label_alternative =         label;  
      while_list = [];        
    }

  module Simple_to_string = struct
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
      | Int_binop_div     ->   "computer_div_" 
      | Int_binop_mod     ->   "computer_mod_" 
      | Int_binop_bin_and -> "bin_land"
      | Int_binop_bin_or  -> "bin_lor" 
      | Int_binop_bin_xor -> "bin_xor" 
      | Int_binop_bin_shl -> "bin_lsr" 
      | Int_binop_bin_shr -> "bin_lsl" 

    let bool_binary_operator = function
      | Bool_binop_equals            -> "="   
      | Bool_binop_notequals         -> "<>"      
      | Bool_binop_strictly_greater  -> ">"             
      | Bool_binop_strictly_lower    -> "<"           
      | Bool_binop_equal_or_greater  -> ">="             
      | Bool_binop_equal_or_lower    -> "<="           

    let get_int_at_buffer = function
      | Get_native         it -> spr "get-native-%s" (integer_type it)
      | Get_big_endian     it -> spr "get-bigend-%s" (integer_type it)
      | Get_little_endian  it -> spr "get-ltlend-%s" (integer_type it)

    let buffer_type = function
      | Type_sized_buffer   i -> spr "buffer-of-size-[%d]" i
      | Type_pointer          -> spr "pointer"
      | Type_sizable_buffer s -> failwith "ERROR"

  end

  let integer_type compiler t =
    Atom (Simple_to_string.integer_type t, compiler.atom_type)

  let int_unary_operator compiler o =
    Atom (Simple_to_string.int_unary_operator o, compiler.atom_operator)

  let int_binary_operator compiler o =
    Atom (Simple_to_string.int_binary_operator o, compiler.atom_operator)

  let bool_binary_operator compiler o =
    Atom (Simple_to_string.bool_binary_operator o, compiler.atom_operator)

  let get_int_at_buffer compiler o =
    Atom (Simple_to_string.get_int_at_buffer o, compiler.atom_funciton)

  let variable_name ?(logical=false) compiler o =
    let name s = if logical then s else sprintf "!%s" s in
    Atom (name o, compiler.atom_literal)

  let easy_int_binary_operator = function
    | Int_binop_add     -> true 
    | Int_binop_sub     -> true 
    | Int_binop_mul     -> true 
    | Int_binop_div     -> false
    | Int_binop_mod     -> false
    | Int_binop_bin_and -> false
    | Int_binop_bin_or  -> false
    | Int_binop_bin_xor -> false
    | Int_binop_bin_shl -> false
    | Int_binop_bin_shr -> false

  let get_in_at_buffer_size = function
    | Get_native        o
    | Get_big_endian    o
    | Get_little_endian o ->
      begin match o with
      | Type_uint8       ->  1 
      | Type_uint16      ->  2
      | Type_uint32      ->  4
      | Type_uint64      ->  8 
      | Type_uint_native ->  8
      end

  let rec buffer_type compiler o =
    Atom (Simple_to_string.buffer_type o, compiler.atom_type)

  and int_expression ?(logical=false) compiler = function
    | Int_expr_unary  (op, ex)        ->
      List (compiler.list_expression,
            [ int_unary_operator compiler op; int_expression ~logical compiler ex])
    | Int_expr_binary (op, ea, eb) when easy_int_binary_operator op -> 
      List (compiler.list_expression,
        [(int_expression ~logical compiler ea);
         (int_binary_operator compiler op);
         (int_expression ~logical compiler eb)])
    | Int_expr_binary (op, ea, eb) -> 
      List (compiler.list_expression,
            [(int_binary_operator compiler op);
             (int_expression ~logical compiler ea);
             (int_expression ~logical compiler eb)])
    | Int_expr_variable  v            -> variable_name ~logical compiler v
    | Int_expr_buffer_content
        (get_int_at_buffer,
         Buffer_expr_offset (Buffer_expr_variable v, int_expr)) ->
      let access =
        let size = get_in_at_buffer_size get_int_at_buffer in
        Int_expr_binary (Int_binop_add, 
                         Int_expr_literal (Int64.of_int (size - 1)),
                         int_expr) in
      (* size - 1 -> because "int_expr" is already the index of the 1st byte *)
      List (compiler.list_expression,
            [ Atom ("buffer_access ", compiler.atom_operator);
             int_expression ~logical compiler access])
    | Int_expr_buffer_content _ as s->
      failwith 
        (sprintf "Forbiden buffer access expression: %s" 
           (To_string.int_expression s))
    | Int_expr_literal        i64     ->
      Atom (Int64.to_string i64, compiler.atom_literal)

  and buffer_expression compiler = function
    | Buffer_expr_variable v         -> variable_name compiler v
    | Buffer_expr_offset (bex, iex)  -> 
      List (compiler.list_expression,
            [buffer_expression compiler bex;
             Atom (":>", compiler.atom_operator);
             int_expression compiler iex])

  and bool_expression ?(logical=false) compiler = function
    | Bool_expr_true   -> Atom ("true", compiler.atom_literal)
    | Bool_expr_false  -> Atom ("false", compiler.atom_literal)
    | Bool_expr_variable v -> variable_name ~logical compiler v
    | Bool_expr_and        (a, b)     ->
      List (compiler.list_expression,
            [bool_expression ~logical compiler a;
             Atom ((if logical then "and" else "&&"), compiler.atom_operator);
             bool_expression ~logical compiler b])
    | Bool_expr_or         (a, b)     ->
      List (compiler.list_expression,
            [bool_expression ~logical compiler a;
             Atom ((if logical then "or" else "||"), compiler.atom_operator);
             bool_expression ~logical compiler b])
    | Bool_expr_not        ex         ->
      List (compiler.list_expression,
            [Atom ("not", compiler.atom_operator);
             bool_expression ~logical compiler ex])
    | Bool_expr_binary_int (op, a, b) ->
      List (compiler.list_expression,
            [(int_expression ~logical compiler a);
             (bool_binary_operator compiler op);
             (int_expression ~logical compiler b)])

  let typed_expression compiler = function
    | Typed_int    (t, e) -> int_expression compiler e
    | Typed_bool       e  -> bool_expression compiler e
    | Typed_buffer (t, e) -> buffer_expression compiler e

  let typed_variable_kind compiler = function
    | Kind_int     t -> Atom ("ref (-42)", compiler.atom_type)
    | Kind_bool      -> Atom ("bool", compiler.atom_type)
    | Kind_buffer  t ->  Atom ("ref 807738", compiler.atom_type)

  let kwd compiler s = Atom (s, compiler.atom_keyword)

  let logic compiler = function
    | Logic_bool_expr b -> bool_expression ~logical:true compiler b
    | Logic_variability (i,v) ->
      List (compiler.list_variability,
            [
              Label ((kwd compiler "invariant", compiler.label_logic),
                     bool_expression ~logical:true compiler i);
              Label ((kwd compiler "variant", compiler.label_logic),
                     int_expression ~logical:true compiler v);])


  let _make_stmt compiler l = List (compiler.list_statement, l)
  let _make_labeled_stmt compiler lb l =
    Label ((lb, compiler.label_statement), _make_stmt compiler l)

  let rec statement_annotation compiler = function
    | Annot_comment s -> List (compiler.list_comment, [Atom (s, compiler.atom_comment)])
    | Annot_specify l ->  List (compiler.list_specification, [logic compiler l])
    | Annot_alternative (`C, s) -> 
      List (compiler.list_comment, 
            [Atom ("C:", compiler.atom_comment);
             statement compiler s])
    | Annot_alternative (`Why, s) -> failwith "Shouldn't be here"

  and statement compiler = 
    let stmt = _make_stmt compiler in
    let labeled_stmt = _make_labeled_stmt compiler in
    function 
    | Do_nothing                 -> kwd compiler "(* NOP *) void"
    | Do_block          e        -> 
      List (compiler.list_block, Ls.map (statement compiler) e)
    | Do_if            (e, a, b) ->
      List (compiler.list_statements,
            [ Label ((kwd compiler "if", compiler.label_alternative),
                     bool_expression compiler e);
              Label ((kwd compiler "then", compiler.label_alternative),
                     statement compiler a);
              Label ((kwd compiler "else", compiler.label_alternative),
                     statement compiler b);])
    | Do_while_loop    (s, e, a)    -> 
      let exception_name = sprintf "Exit_while_%d" (Hash.string s) in
      compiler.while_list <- exception_name :: compiler.while_list;
      Label (
        (kwd compiler "try while", compiler.label_alternative),
        (List (compiler.list_sentence,
               [ bool_expression compiler e;
                 kwd compiler "do" ] 
               @ [statement compiler a]
               @ [kwd compiler 
                     (sprintf "done with %s -> void end" exception_name)])))
    | Do_exit_while s ->
      stmt [kwd compiler (sprintf "raise Exit_while_%d" (Hash.string s))]
    | Do_assignment  (a, b) -> 
      begin match b with 
      | Typed_int (_, e) ->
        labeled_stmt (kwd compiler a) [
          Atom (":=", compiler.atom_operator);
          typed_expression compiler b;
        ]
      | _ ->
        kwd compiler (sprintf "(* Removed assignement (%s) *) void" a)
      end
    | Do_external_assignment  (a, f, args) -> 
      failwith "Do_external_assignment -> Why: not implment{ed,able}"
    | Do_declaration t -> 
      begin match t.kind with
      | Kind_int _ ->
        labeled_stmt (kwd compiler "let ") [
          kwd compiler t.name;
          kwd compiler "=";
          typed_variable_kind compiler t.kind;
          kwd compiler "in void"
        ]
      | _ ->
        kwd compiler (sprintf "(* Removed declaration (%s) *) void" t.name)
      end
    | Do_log (f, l) -> (kwd compiler "(* Log *) void")
    | Do_annotated_statement (Annot_alternative (`Why, s), st) ->
      statement compiler s
    | Do_annotated_statement (annot, st) ->
      List (compiler.list_statements,
            [ statement_annotation compiler annot;
              statement compiler st ])

  let mark_packet_length = Statement.cmt "why-packet-length-parameter"

  let statement_to_string s =
    let compiler = (default_compiler ()) in
    let packet_length_parameter =
      let res = ref "" in
      let f = function
        | Do_annotated_statement (Annot_alternative (`Why, mpl), 
                                  Do_declaration v)
            when mpl = mark_packet_length -> res := v.name; true
        | _ -> false in
      match Ls.find_opt (Statement.unblock s) ~f with
      | None -> failwith "Did not find the packet length mark; Why is useless."
      | Some s -> !res
    in

    let why =
      _make_labeled_stmt compiler 
        (kwd compiler "let statement () =") [
          kwd compiler (sprintf "{ %s >= 0 }" packet_length_parameter);
          statement compiler s] in
    (sprintf "include \"divisions.why\"\n\
              %s\n\
              parameter bin_mod : a:int -> b:int -> {} int { 0 <= result <= b }\n\
              parameter bin_land: a:int -> b:int -> {} int { 0 <= result }\n\
              parameter bin_lor : a:int -> b:int -> {} int { 0 <= result }\n\
              parameter bin_xor : a:int -> b:int -> {} int { 0 <= result }\n\
              parameter bin_lsr : a:int -> b:int -> {} int { 0 <= result }\n\
              parameter bin_lsl : a:int -> b:int -> {} int { 0 <= result }\n\
              parameter %s: int ref\n\
              parameter buffer_access:\n\
              \   last_index:int ->\n\
              \   { 0 <= last_index < %s } int reads %s { result >= 0 }\n\
              %s {true}"
       (Str.concat "\n" (Ls.map compiler.while_list ~f:(sprintf "exception %s")))
       packet_length_parameter
       packet_length_parameter
       packet_length_parameter
       (Easy_format.Pretty.to_string  why))
      

end


module To_ocaml_string = struct


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
    list_specification: string * string * string * list_param;
    label_statement: label_param;
    label_logic: label_param;
    label_alternative: label_param;

    mutable while_list: string list;
  }
  let default_compiler () =
    let list_for_statements = 
      { list with
        stick_to_label = true;
        align_closing = true;
        wrap_body = `Force_breaks;
        separators_stick_left = true;
        space_before_separator = false;
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
      list_statement = ("", "", "", list_for_sentence);
      list_sentence = ("", "", "", list_for_sentence);
      list_coma = ("(", ",", ")", list_for_sentence);
      list_typed_expression = ("[", ":", "]", list_for_sentence);              
      list_block =  ("begin", ";", "end", {list_for_statements with indent_body = 2;} );
      list_statements =  ("", "", "", list_for_statements);
      list_variability =  ("", "", "", list);
      list_comment =  ("(*", "", "*)", list);
      list_specification =  ("{", "", "}", list);
      label_statement =               label;    
      label_logic =               label;    
      label_alternative =         label;  
      while_list = [];        
    }
  let fail compiler s =
    failwith (sprintf "Promiwag_stiel.To_Ocaml: ERROR: %s" s)

  module Simple_to_string = struct
    let spr = Printf.sprintf

    let  integer_type = function
      | Type_uint8       -> "uint8"      
      | Type_uint16      -> "uint16"     
      | Type_uint32      -> "uint32"     
      | Type_uint64      -> "uint64"     
      | Type_uint_native -> "uint_native"

    let int_unary_operator = function
      | Int_unary_minus             -> "Integer.minus"
      | Int_unary_plus              -> "Integer.plus"

    let int_binary_operator = function
      | Int_binop_add     ->   "Integer.add" 
      | Int_binop_sub     ->   "Integer.sub" 
      | Int_binop_mul     ->   "Integer.mul" 
      | Int_binop_div     ->   "Integer.div" 
      | Int_binop_mod     ->   "Integer.rem" 
      | Int_binop_bin_and -> "Integer.bin_and"
      | Int_binop_bin_or  -> "Integer.bin_or" 
      | Int_binop_bin_xor -> "Integer.bin_xor" 
      | Int_binop_bin_shl -> "Integer.bin_lsl" 
      | Int_binop_bin_shr -> "Integer.bin_lsr" 

    let bool_binary_operator = function
      | Bool_binop_equals            -> "Integer.eq"   
      | Bool_binop_notequals         -> "Integer.ne"      
      | Bool_binop_strictly_greater  -> "Integer.gt"             
      | Bool_binop_strictly_lower    -> "Integer.lt"           
      | Bool_binop_equal_or_greater  -> "Integer.ge"             
      | Bool_binop_equal_or_lower    -> "Integer.le"           

    let get_int_at_buffer = function
      | Get_native         it -> spr "Unsafe_buffer.get_native_%s" (integer_type it)
      | Get_big_endian     it -> spr "Unsafe_buffer.get_bigend_%s" (integer_type it)
      | Get_little_endian  it -> spr "Unsafe_buffer.get_ltlend_%s" (integer_type it)


  end

  let integer_type compiler t =
    Atom (Simple_to_string.integer_type t, compiler.atom_type)

  let int_unary_operator compiler o =
    Atom (Simple_to_string.int_unary_operator o, compiler.atom_operator)

  let int_binary_operator compiler o =
    Atom (Simple_to_string.int_binary_operator o, compiler.atom_operator)

  let bool_binary_operator compiler o =
    Atom (Simple_to_string.bool_binary_operator o, compiler.atom_operator)

  let get_int_at_buffer compiler o =
    Atom (Simple_to_string.get_int_at_buffer o, compiler.atom_funciton)

  let variable_name ~ref compiler o =
    let name s = if ref then sprintf "!%s" s else s in
    Atom (name o, compiler.atom_literal)

  let rec  int_expression  compiler = function
    | Int_expr_unary  (op, ex)        ->
      List (compiler.list_expression,
            [ int_unary_operator compiler op; int_expression compiler ex])
    | Int_expr_binary (op, ea, eb) -> 
      List (compiler.list_expression,
        [(int_binary_operator compiler op);
         (int_expression compiler ea);
         (int_expression compiler eb)])
    | Int_expr_variable  v            -> variable_name ~ref:true compiler v
    | Int_expr_buffer_content (get, buffer) ->
      List (compiler.list_expression,
        [(get_int_at_buffer compiler get);
         (buffer_expression  compiler buffer);])
    | Int_expr_literal        i64     ->
      List (compiler.list_expression,
            [ Atom ("Integer.of_int64", compiler.atom_literal);
              Atom ((Int64.to_string i64) ^ "L", compiler.atom_literal)])

  and buffer_expression compiler = function
    | Buffer_expr_variable v         -> variable_name ~ref:true compiler v
    | Buffer_expr_offset (bex, iex)  -> 
      List (compiler.list_expression,
            [ Atom ("Unsafe_buffer.offset", compiler.atom_operator);
              buffer_expression compiler bex;
              int_expression compiler iex])

  and bool_expression compiler = function
    | Bool_expr_true   -> Atom ("true", compiler.atom_literal)
    | Bool_expr_false  -> Atom ("false", compiler.atom_literal)
    | Bool_expr_variable v -> variable_name ~ref:true compiler v
    | Bool_expr_and        (a, b)     ->
      List (compiler.list_expression,
            [bool_expression compiler a;
             Atom ("&&", compiler.atom_operator);
             bool_expression compiler b])
    | Bool_expr_or         (a, b)     ->
      List (compiler.list_expression,
            [bool_expression compiler a;
             Atom ("||", compiler.atom_operator);
             bool_expression compiler b])
    | Bool_expr_not        ex         ->
      List (compiler.list_expression,
            [Atom ("not", compiler.atom_operator);
             bool_expression compiler ex])
    | Bool_expr_binary_int (op, a, b) ->
      List (compiler.list_expression,
            [ (bool_binary_operator compiler op);
              (int_expression compiler a);
              (int_expression compiler b)])

  let typed_expression compiler = function
    | Typed_int    (t, e) -> int_expression compiler e
    | Typed_bool       e  -> bool_expression compiler e
    | Typed_buffer (t, e) -> buffer_expression compiler e

  let typed_variable_kind compiler = function
    | Kind_int     t -> Atom ("ref (-42L)", compiler.atom_type)
    | Kind_bool      -> Atom ("ref false", compiler.atom_type)
    | Kind_buffer (Type_sized_buffer   i) ->
      Atom (sprintf "ref (Unsafe_buffer.create_sized %d)" i, compiler.atom_type)
    | Kind_buffer (Type_pointer         ) ->
      Atom ("ref (Unsafe_buffer.create_pointer ())", compiler.atom_type)
    | Kind_buffer (Type_sizable_buffer _) -> failwith "ERROR"

  let kwd compiler s = Atom (s, compiler.atom_keyword)

  let _make_stmt compiler l = List (compiler.list_statement, l)
  let _make_labeled_stmt compiler lb l =
    Label ((lb, compiler.label_statement), _make_stmt compiler l)


  let printf_of_log compiler format te_list =
    let escape = "AROBASEESCAPE4732992387737238292933" in
    let fmt = ref (Str.multi_replace ~str:format ~sub:"@@" ~by:escape) in
    let arg_list =
      Ls.map te_list ~f:(fun te ->
        let type_fmt lfmt =
          let ux =
            match te with
            | Typed_int (Type_uint8      , _) -> ("%Lu", "%02Lx")
            | Typed_int (Type_uint16     , _) -> ("%Lu" , "%04Lx")
            | Typed_int (Type_uint32     , _) -> ("%Lu"  , "%08Lx")
            | Typed_int (Type_uint64     , _) -> ("%Lu", "%16Lx")
            | Typed_int (Type_uint_native, _) -> ("%Lu" , "%16Lx")
            | Typed_bool       _  -> ("%B", "%B")
            | Typed_buffer (t, _) ->  ("%a", "%a") in
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
          (kwd compiler (sprintf "%S" (To_string.typed_expression te)))
        |  s -> 
          fail compiler (sprintf "Do_log: should not be here (arg_list): %s" s)
      ) in
    fmt := (Str.multi_replace ~str:!fmt ~sub:escape ~by:"@");
    _make_labeled_stmt compiler 
      (kwd compiler "Log.printf")
      ((kwd compiler (sprintf "%S" (!fmt ^ "%!"))) :: arg_list)

  let rec statement_annotation compiler = function
    | Annot_comment s ->
      List (compiler.list_comment, 
            [Atom (sprintf "(* %s *)" s, compiler.atom_comment)])
    | Annot_specify _ 
    | Annot_alternative _ -> 
      List (compiler.list_comment, 
            [Atom ("(* *)", compiler.atom_comment)])

  and statement compiler = 
    let stmt = _make_stmt compiler in
    let labeled_stmt = _make_labeled_stmt compiler in
    function 
    | Do_nothing                 -> kwd compiler "(* NOP *) ()"
    | Do_block          e        -> 
      List (compiler.list_block, Ls.map (statement compiler) e)
    | Do_if            (e, a, b) ->
      List (compiler.list_statements,
            [ Label ((kwd compiler "if", compiler.label_alternative),
                     bool_expression compiler e);
              Label ((kwd compiler "then", compiler.label_alternative),
                     statement compiler a);
              Label ((kwd compiler "else", compiler.label_alternative),
                     statement compiler b);])
    | Do_while_loop    (s, e, a)    -> 
      let exception_name = sprintf "Exit_while_%d" (Hash.string s) in
      compiler.while_list <- exception_name :: compiler.while_list;
      Label (
        (kwd compiler "begin try while", compiler.label_alternative),
        (List (compiler.list_sentence,
               [ bool_expression compiler e;
                 kwd compiler "do" ] 
               @ [statement compiler a]
               @ [kwd compiler 
                     (sprintf "done with %s -> () end" exception_name)])))
    | Do_exit_while s ->
      stmt [kwd compiler (sprintf "raise Exit_while_%d" (Hash.string s))]
    | Do_assignment  (a, b) -> 
      labeled_stmt (kwd compiler a) [
        Atom (":=", compiler.atom_operator);
        typed_expression compiler b;
      ]
    | Do_external_assignment  (a, f, args) -> 
      labeled_stmt (kwd compiler a) (
        [Atom (":=", compiler.atom_operator);
         Atom (f, compiler.atom_operator);] @
          (Ls.map (typed_expression compiler) args))
    | Do_declaration t -> 
      labeled_stmt (kwd compiler "let ") [
        kwd compiler t.name;
        kwd compiler "=";
        typed_variable_kind compiler t.kind;
        kwd compiler "in ()"
      ]
    | Do_log (f, l) -> printf_of_log compiler f l
    | Do_annotated_statement (annot, st) ->
      List (compiler.list_statements,
            [ statement_annotation compiler annot;
              statement compiler st ])

  let default_prelude = 
begin
"

module Integer = struct


  include Int64

  let of_int64 x = x

  let eq a b = (compare a b)  = 0
  let ne a b = (compare a b) <> 0    
  let gt a b = (compare a b) >  0           
  let lt a b = (compare a b) <  0         
  let ge a b = (compare a b) >= 0           
  let le a b = (compare a b) <= 0         

  let minus x = failwith \"Not unsigned operation\"
  let plus x = x
  let sub a b = 
    let r = Int64.sub a b in
    if lt r 0L then 0L else r
  let bin_and = logand
  let bin_or  = logor
  let bin_xor = logxor
  let bin_lsl a b = shift_left a (to_int b)
  let bin_lsr a b = shift_right_logical a (to_int b)

end

module Unsafe_buffer = struct

  type t = {
    underlying: string;
    beginning: int;
  }
  let create s = { underlying = s; beginning = 0}
  let create_pointer () = create \"\"

  let offset s o =
    let ofs = Integer.to_int o in
    { s with beginning = ofs + s.beginning}

  let get_native_uint8       b =  failwith \"get native not implemented\"
  let get_native_uint16      b =  failwith \"get native not implemented\"
  let get_native_uint32      b =  failwith \"get native not implemented\"
  let get_native_uint64      b =  failwith \"get native not implemented\"
  let get_native_uint_native b =  failwith \"get native not implemented\"

  let get b i = Integer.of_int (int_of_char (String.unsafe_get b.underlying (i + b.beginning)))
  open Integer
  let get_bigend_uint8       b = let g = get b in  (g 0)
  let get_bigend_uint16      b = let g = get b in  add (bin_lsl (g 0) 8L) (g 1)
  let get_bigend_uint32      b = let g = get_bigend_uint16 in  add (bin_lsl (g b) 16L) (g (offset b 2L))
  let get_bigend_uint64      b = let g = get_bigend_uint32 in  add (bin_lsl (g b) 32L) (g (offset b 4L))
  let get_bigend_uint_native = if Sys.word_size = 32 then  get_bigend_uint32 else  get_bigend_uint64
  let get_ltlend_uint8       b = let g = get b in  (g 0)
  let get_ltlend_uint16      b = let g = get b in  add (bin_lsl (g 1) 8L) (g 0)
  let get_ltlend_uint32      b = let g = get_bigend_uint16 in  add (bin_lsl (g (offset b 2L)) 16L) (g b)
  let get_ltlend_uint64      b = let g = get_bigend_uint32 in  add (bin_lsl (g (offset b 4L)) 32L) (g b)
  let get_ltlend_uint_native b =  if Sys.word_size = 32 then  get_ltlend_uint32 else  get_ltlend_uint64

end

module Log = Printf

"
end


  let statement_to_string 
      ?(function_name="stiel_code") 
      ?(with_default_prelude=false) s =
    let compiler = (default_compiler ()) in

    let undefined =
      let names = ref [] in
      let decls = ref [] in
      Visit.statement
        (Visit.compiler ()
           ~on_variables:(fun s -> names := s :: !names)
           ~on_variable_declarations:(fun s -> decls := s :: !decls)) s;
      Ls.filter !names ~f:(fun s -> not (Ls.mem s !decls))
    in

    let code =
      let args =
        match undefined with
        | [] -> "()"
        | l -> sprintf "~%s" (Str.concat " ~" l)
      in
      _make_labeled_stmt compiler 
        (kwd compiler (sprintf "let %s %s =" function_name args))
        [ statement compiler s]
    in
    (sprintf "%s\n%s\n%s\n"
       (if with_default_prelude then default_prelude else "")
       (Str.concat "\n" (Ls.map compiler.while_list ~f:(sprintf "exception %s")))
       (Easy_format.Pretty.to_string  code))


end

