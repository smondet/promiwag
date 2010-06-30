open Promiwag_std

module Packet_structure = struct

  type int_operator =
    | Op_add
    | Op_sub
    | Op_mul
    | Op_div

  let caml_op_of_size_op = function
    | Op_add -> (+) | Op_sub -> (-) | Op_mul -> ( * ) | Op_div -> (/)
  let string_of_size_op = function
    | Op_add -> "+" | Op_sub -> "-" | Op_mul -> "*" | Op_div -> "/"

  type size =
    | Size_fixed of int
    | Size_variable of string
    | Size_binary_expression of int_operator * size * size
    | Size_alignment of int * size
    | Size_offset_of of string
    | Size_unknown

  let size_int i = Size_fixed i
  let size_var v = Size_variable v

  let rec string_of_size = function
    | Size_fixed o -> sprintf "%d" o
    | Size_variable v -> v
    | Size_binary_expression (op, size_a, size_b) ->
      sprintf "(%s %s %s)" (string_of_size size_a)
        (string_of_size_op op) (string_of_size size_b)
    | Size_alignment (i, s) -> sprintf "align%d(%s)" i (string_of_size s)
    | Size_offset_of o -> sprintf "offset_of(%s)" o
    | Size_unknown ->  "[Unknown]"

  let rec parse_size = function
    | `int i -> Size_fixed i
    | `var v -> Size_variable v
    | `add (a, b) -> Size_binary_expression (Op_add, parse_size a, parse_size b)
    | `sub (a, b) -> Size_binary_expression (Op_sub, parse_size a, parse_size b)
    | `mul (a, b) -> Size_binary_expression (Op_mul, parse_size a, parse_size b)
    | `div (a, b) -> Size_binary_expression (Op_div, parse_size a, parse_size b)
    | `align64 e -> Size_alignment (8, parse_size e)
    | `align32 e -> Size_alignment (4, parse_size e)
    | `align16 e -> Size_alignment (2, parse_size e)
    | `align8 e -> Size_alignment  (1, parse_size e)
    | `offset v -> Size_offset_of v
    | `size s -> s

  let size = parse_size

  type content_type =
    | Type_unsigned_integer of size
    | Type_signed_integer of size
    | Type_little_endian of content_type
    | Type_string of size

  let rec string_of_content_type ?(little_endian=false) ct =
    let lestr = if little_endian then " LE" else "" in
    match ct with
    | Type_unsigned_integer s -> sprintf "uint of %s bits%s" (string_of_size s) lestr
    | Type_signed_integer s -> sprintf "sint of %s bits%s" (string_of_size s) lestr
    | Type_little_endian ct -> string_of_content_type ~little_endian:true ct
    | Type_string s -> sprintf "string of %s Bytes%s" (string_of_size s) lestr

  let fixed_int ?(unsigned=true) ?(little_endian=false) s = 
    let t = 
      if unsigned then Type_unsigned_integer (Size_fixed s) 
      else Type_signed_integer (Size_fixed s)
    in
    if little_endian then Type_little_endian t else t

  let fixed_string i = Type_string (Size_fixed i)
  let variable_string v = Type_string (Size_variable v)

  type content_item =
    | Item_field of string * content_type

  let field name the_type = Item_field (name, the_type)
  let fixed_int_field name size =
    Item_field (name, Type_unsigned_integer (Size_fixed size))
  let string_field name size = Item_field (name,  Type_string size)


  let payload ?size ?(name="payload") () =
    let actual_size = 
      match size with None -> Size_unknown | Some s -> s in
    Item_field (name, Type_string actual_size)

  type format = content_item list
  type packet = string * format

  let packet_format name format = ((name, format): packet)

end

module Packet_database = struct

  type t = (string, Packet_structure.packet) Ht.t

  let add t (name, packet) = Ht.add t name (name, packet)

  let of_list l = 
    let t = Ht.create 42 in
    Ls.iter (fun p -> add t p) l;
    t

  let get_format (t:t) name =
    try let _, fmt = Ht.find t name in fmt with
    | Not_found ->
      failwith (sprintf "ERROR: Meta_packet.Packet_database.find \
                   packet format \"%s\" not found" name)

end

module Parser_generator = struct

  (*
    a request of offset should be a difference from current packet
    a request of pointer should be a C-like pointer
    a value is for now an unsigned integer of minimal size
  *)
  type request = [ 
  | `value of string
  | `pointer of string
  | `offset of string
  ]


  module Stage_1 = struct 
      
    open Packet_structure

    let cmp_str a b = (Str.compare a b) = 0

    let error_prefix = "Meta_packet.Parser_generator.Stage_1"
    let to_do s =
      failwith (sprintf "%s; %s: NOT IMPLEMENTED" error_prefix s)
    let fail s =
      failwith (sprintf "%s: ERROR %s" error_prefix s)

    type dependency =
      | Depend_on_value_of of string
      | Depend_on_offset_of of string
      | Depend_on_pointer_to of string
      | Depend_on_unknown

    let dependencies_of_size sz =
      let rec tree_descent deps = function
        | Size_fixed s -> deps
        | Size_variable v -> (Depend_on_value_of v :: deps)
        | Size_binary_expression (op, size_a, size_b) ->
          (tree_descent deps size_a) @ (tree_descent deps size_b)
        | Size_alignment (i, s) -> tree_descent deps s
        | Size_offset_of v -> (Depend_on_offset_of v :: deps)
        | Size_unknown -> (Depend_on_unknown :: deps)
      in
      (tree_descent [] sz)

    let string_of_dependency = function
      | Depend_on_value_of o -> sprintf "(value-of: %s)" o
      | Depend_on_offset_of o -> sprintf "(offset-of: %s)" o
      | Depend_on_pointer_to o -> sprintf "(pointer-to: %s)" o
      | Depend_on_unknown -> "(depends-on-unknown)"
        
    type computation_item =
      | Compute_byte_offset of size
      | Compute_bit_offset of size

    let rec computation_of_content_type = function
      | Type_little_endian ct -> computation_of_content_type ct
      | Type_unsigned_integer sz
      | Type_signed_integer sz -> 
        (Compute_bit_offset sz, dependencies_of_size sz)
      | Type_string sz -> 
        (Compute_byte_offset sz, dependencies_of_size sz)
          
    let string_of_computation_item = function
      | Compute_byte_offset o -> sprintf "(byte-offset: %s)" (string_of_size o)
      | Compute_bit_offset o -> sprintf "(bit-offset: %s)" (string_of_size o)


    type final_computation =
      | Finally_get_integer of [`big | `little] * [`signed | `unsigned] * size
      | Finally_fail of [`string]

    let string_of_final_computation = function
      | Finally_get_integer (`big, `signed, s) -> 
        sprintf "(get-integer %s, signed, big endian)" (string_of_size s)
      | Finally_get_integer (`big, `unsigned, s) -> 
        sprintf "(get-integer %s, unsigned, big endian)" (string_of_size s)
      | Finally_get_integer (`little, `signed, s) -> 
        sprintf "(get-integer %s, signed, little endian)" (string_of_size s)
      | Finally_get_integer (`little, `unsigned, s) -> 
        sprintf "(get-integer %s, unsigned, little endian)" (string_of_size s)
      | Finally_fail `string -> "fail-on-string"

    let rec final_computation_of_content_type ?(endianism=`big) = function
      | Type_little_endian ct -> 
        final_computation_of_content_type ~endianism:`little ct
      | Type_unsigned_integer sz ->
        (Finally_get_integer (endianism, `unsigned, sz),
         dependencies_of_size sz)
      | Type_signed_integer sz ->
        (Finally_get_integer (endianism, `unsigned, sz),
         dependencies_of_size sz)
      | Type_string sz -> (Finally_fail `string, [])

    let rec propagate_constants_in_size = function
      | Size_fixed s -> Size_fixed s
      | Size_variable v -> Size_variable v
      | Size_binary_expression (op, size_a, size_b) ->
        let propagated_a = propagate_constants_in_size size_a in
        let propagated_b = propagate_constants_in_size size_b in
        begin match propagated_a, propagated_b with
        | Size_fixed a, Size_fixed b ->
          Size_fixed (caml_op_of_size_op op a b)
        | sa, sb ->
          Size_binary_expression (op, sa, sb)
        end
      | Size_alignment (al, s) ->
        begin match propagate_constants_in_size s with
        | Size_fixed i -> Size_fixed (i + (i mod al))
        | sz -> Size_alignment (al, sz)
        end
      | Size_offset_of v -> Size_offset_of v
      | Size_unknown -> Size_unknown

    let aggregate_computations computations = 
      let byte_offset, bit_offset =
        let f (prev_byte, prev_bit) = function
          | Compute_byte_offset s ->
            (Size_binary_expression (Op_add, prev_byte, s), prev_bit)
          | Compute_bit_offset s ->
            (prev_byte, Size_binary_expression (Op_add, prev_bit, s)) in
        Ls.fold_left ~f computations ~init:(Size_fixed 0, Size_fixed 0) in
      let total_byte_offset, in_byte_bit_offset =
        size (`add (`size byte_offset, `div (`size bit_offset, `int 8))),
        size (`sub (`size bit_offset,
                    `mul (`int 8, `div (`size bit_offset, `int 8))))
      in
      let propagated_byte_ofs, propagated_bit_ofs =
        propagate_constants_in_size total_byte_offset,
        propagate_constants_in_size in_byte_bit_offset in
      (propagated_byte_ofs, propagated_bit_ofs)

    let find_field_in_packet packet_format field_name =
      let rec f computations dependencies field_name = function
        | [] ->
          fail (sprintf "Field %s not found" field_name)
        | (Item_field (name, tp)) :: l ->
          if cmp_str name field_name then
            let fcomp, fdeps = final_computation_of_content_type tp in
            (fcomp, fdeps, Ls.rev computations, Ls.rev dependencies)
          else
            let comp, dep = computation_of_content_type tp in
            f (comp :: computations) (dep :: dependencies) field_name l
      in
      (f [] [] field_name packet_format)

    type needer = [`request | `other  of stage_1_compiled_expression]
    and need = [`value of needer | `pointer of needer | `offset of needer]
    and stage_1_compiled_expression = {  (* TODO find a better name... *)
      s1_field: string;
      s1_byte_offset: size;
      s1_bit_offset: size;
      s1_final: final_computation;
      s1_dependencies: dependency list;
      mutable s1_needed_as_by: need list;
    }

    let explode_stage_1_compiled_expression ce =
      (ce.s1_field, 
       ce.s1_byte_offset,
       ce.s1_bit_offset,
       ce.s1_final,
       ce.s1_dependencies,
       ce.s1_needed_as_by)

    let stage_1_compile_field needed_as_by packet_format field_name =
      let final_comp, final_deps, comps, deps =
        find_field_in_packet packet_format field_name in
      let total_deps = final_deps @ (Ls.flatten deps) in
      let byte_ofs, bit_ofs = aggregate_computations comps in
      {s1_field = field_name; s1_byte_offset = byte_ofs; s1_bit_offset = bit_ofs;
       s1_final = final_comp; s1_dependencies = total_deps;
       s1_needed_as_by = [needed_as_by]; }

    let stage_1_compile_dependency packet_format (needed_by:needer) = function
      | Depend_on_value_of f ->
        stage_1_compile_field (`value needed_by) packet_format f
      | Depend_on_offset_of f ->
        stage_1_compile_field (`offset needed_by) packet_format f
      | Depend_on_pointer_to f ->
        stage_1_compile_field (`pointer needed_by) packet_format f
      | Depend_on_unknown ->
        fail "stage_1_compile_dependency: should not be here"

    let dependencies_of_request: request list -> dependency list =
      Ls.map ~f:(function 
        | `value f -> Depend_on_value_of f
        | `offset f -> Depend_on_offset_of f
        | `pointer f -> Depend_on_pointer_to f)

    type result = {
      packet_format: packet;
      request_list: request list;
      compiled_expressions: stage_1_compiled_expression list;
    }

    let compile_with_dependencies
        ?(max_depth=42) ~packet_format (request:request list) =
      let first_dependencies = dependencies_of_request request in
      let stage_compiled_things = ref [] in
      let die_on_max_depth depender d =
        if d > max_depth then 
          fail (sprintf "Stage 1: Maximal dependency reached for %s"
                  (match depender with 
                  | `request -> "request (this should never happen)"
                  | `other c -> c.s1_field)) in
      let rec go_deeper depth depender = function
        | [] -> (* Done! *) ()
        | (Depend_on_value_of fname as d) :: l
        | (Depend_on_pointer_to fname as d) :: l
        | (Depend_on_offset_of fname as d) :: l ->
          die_on_max_depth depender depth;
          let f ct = cmp_str ct.s1_field fname in
          begin match Ls.find_all !stage_compiled_things ~f with
          | [] ->
            let compiled =
              stage_1_compile_dependency (snd packet_format) depender d in
            go_deeper (depth + 1) (`other compiled) compiled.s1_dependencies;
            stage_compiled_things := compiled :: !stage_compiled_things;
          | one :: [] ->
            begin match d with
            | Depend_on_value_of _ ->
              one.s1_needed_as_by <- (`value depender) :: one.s1_needed_as_by;
            | Depend_on_offset_of _ -> 
              one.s1_needed_as_by <- (`offset depender) :: one.s1_needed_as_by;
            | Depend_on_pointer_to _ -> 
              one.s1_needed_as_by <- (`pointer depender) :: one.s1_needed_as_by;
            | Depend_on_unknown -> 
              fail "compile_with_dependencies: Depend_on_unknown"
            end;
          | more -> 
            fail (sprintf "field %s added %d times to stage_compiled_things"
                    fname (Ls.length more))
          end;
          go_deeper depth depender l;
        | Depend_on_unknown :: l ->
          fail (sprintf "Stage 1: Cannot compile unknown dependency of %s"
                  (match depender with 
                  | `request -> "request (this should never happen)"
                  | `other c -> c.s1_field)) 
      in
      go_deeper 0 `request first_dependencies;
      {packet_format = packet_format; 
       request_list = request;
       compiled_expressions = Ls.rev !stage_compiled_things}

    let string_of_stage_1_compiled_expression
        ?(before="") ?(sep_parens=" ") ce =
      sprintf "%s[\"%s\" (byte_offset: %s, bit_offset: %s)%s\
              (final: %s)%s(dependencies: [%s])%s(%s)]"
        before ce.s1_field
        (string_of_size ce.s1_byte_offset) 
        (string_of_size ce.s1_bit_offset) sep_parens
        (string_of_final_computation ce.s1_final) sep_parens
        (Str.concat "; " (Ls.map string_of_dependency ce.s1_dependencies))
        sep_parens
        (Str.concat "; " (Ls.map (function
          | `value `request -> "requested value" 
          | `offset `request -> "requested offset" 
          | `pointer `request -> "requested pointer" 
          | `value (`other c) -> sprintf "value needed by %s" c.s1_field
          | `offset (`other c) -> sprintf "offset needed by %s" c.s1_field
          | `pointer (`other c) -> sprintf "pointer needed by %s" c.s1_field
         ) ce.s1_needed_as_by))

    let string_of_stage_1_compilation_ht
        ?(sep_items="\n") ?(before="") ?(sep_parens="\n  ") ht =
      let l = ref [] in
      Ls.iter (fun s1 -> 
        let expr = 
          string_of_stage_1_compiled_expression ~before ~sep_parens s1 in
        l := expr :: !l;
      ) ht;
      Str.concat sep_items (Ls.rev !l)

    let dump s1 =
      sprintf "{Stage 1 for [%s], on \"%s\" packets:\n%s\n}"
        (Str.concat ", " (Ls.map (function
          | `value f -> sprintf "field %s" f
          | `pointer f -> sprintf "pointer  %s" f
          | `offset f -> sprintf "offset of %s" f) s1.request_list))
        (fst s1.packet_format)
        (string_of_stage_1_compilation_ht
           ~before:"  " ~sep_parens:"\n    " s1.compiled_expressions)

  end

  module Stage_2_C = struct
    
    module C = Promiwag_c_backend.C_LightAST
    module Platform = Promiwag_platform
    open Promiwag_c_backend
    open Packet_structure

    (* Input/Output Types: *)

    type variable_creation_preference =
      [`minimalistically (** Only when absolutely needed, this may mean 
                             duplication of computations. *)
      | `as_needed (** Normal behaviour: create variables when absolutely
                       needed, and to save intermediary results, and avoid
                       duplication of computations. *)
      | `for_all (** Create variables for everything that has to be compiled,
                     in particular, all the typed expressions passed to
                     [make_user_block] will be [`variables name]. *)
      ]

    type c_packet = {
      packet_expression: Typed_expression.t;
      packet_size: Typed_expression.t option;
    }
    let make_packet ?size expression =
      {packet_expression = expression; packet_size = size;}

    exception Compilation_error of string


    (* Internal representations: *)

    type c_compiled =
      | C_variable of Variable.t * Typed_expression.t
      | C_expression of Typed_expression.t
      | C_not_compiled

    type compiled_entity = {
      c_value: c_compiled;
      c_offset: c_compiled;
      c_pointer: c_compiled;
      stage_1_expression: Stage_1.stage_1_compiled_expression;
      buffer_access: C.expression * int;
    }

    type compiler = {
      stage_1: Stage_1.result;
      compiled_entities: (string, compiled_entity) Ht.t;
      target_platform: Promiwag_platform.platform;
      variable_creation_preference: variable_creation_preference;
      mutable location: string option;
    }

        
    let error_prefix = "Meta_packet.Parser_generator.Stage_2_C"
      
    let to_do s =
      failwith (sprintf "%s; %s: NOT IMPLEMENTED" error_prefix s)

    let fail compiler s =
      let field_str = 
        match compiler.location with | None -> "" | Some s -> s in
      failwith (sprintf "%s%s: COMPILER-ERROR %s" error_prefix field_str s)

    let error compiler s =
      let field_str =
        match compiler.location with | None -> "" | Some s -> s in
      raise (Compilation_error (sprintf "%s%s: %s" error_prefix field_str s))


    let set_location_field compiler field =
      compiler.location <- Some (sprintf "{Compiling field  %s]" field)
        
    let get_c_expression compiler cc =
      match cc with
      | C_variable (v, ev) -> Variable.typed_expression v
      | C_expression e -> e
      | _ -> fail compiler "asking for a non-compiled c_thing of entity"
        
    let get_c_dependency compiler needed_as name =
      match Ht.find_opt compiler.compiled_entities name with
      | None -> fail compiler "asking for a non-compiled entitie"
      | Some ce ->
        begin match needed_as with
        | `value ->   get_c_expression compiler ce.c_value
        | `offset ->  get_c_expression compiler ce.c_offset 
        | `pointer -> get_c_expression compiler ce.c_pointer
        end

    let get_c_dependency_expression c n a =
      Typed_expression.expression (get_c_dependency c n a)

    let c_op_of_size_op = function
      | Op_add -> `bin_add
      | Op_sub -> `bin_sub
      | Op_mul -> `bin_mul
      | Op_div -> `bin_div

    let rec compile_size compiler needed_as = function
      | Size_fixed s -> `literal_int s
      | Size_variable v -> get_c_dependency_expression compiler needed_as v
      | Size_binary_expression (op, size_a, size_b) ->
        `binary (c_op_of_size_op op,
                 compile_size compiler needed_as size_a,
                 compile_size compiler needed_as size_b)
      | Size_alignment (i, s) ->
        let c = compile_size compiler needed_as s in
        `binary (`bin_add, c, `binary (`bin_mod, c, `literal_int i))
      | Size_offset_of v ->
        get_c_dependency_expression compiler `offset v
      | Size_unknown -> 
        fail compiler "Trying to compile unknown size"

    let c_offset compiler byte_offset =
     Typed_expression.create ()
       ~expression:(compile_size compiler `value byte_offset)
       ~c_type:(Platform.C.native_uint compiler.target_platform)

    let c_pointer_in_packet compiler packet byte_offset =
      let packet_as_buffer = 
        `cast (`pointer `unsigned_char,
               Typed_expression.expression packet) in
      let c_offset = compile_size compiler `value byte_offset in
      let the_address_at_offset =
        `binary (`bin_add, packet_as_buffer, c_offset) in
      Typed_expression.create ~expression:the_address_at_offset
        ~c_type:(`pointer `unsigned_char) ()
        
    let c_value_type_of_size compiler = function
      | Size_fixed i ->
        Platform.C.fitted_uint compiler.target_platform i
      | _ ->
        (* Emit warning ??? *)
        (Platform.C.native_uint compiler.target_platform,
         Platform.C.size_of_native_uint compiler.target_platform)

    let c_value  compiler pointer bit_offset final =
      let (endianism, signedism, sz) =
        (* [`big | `little] * [`signed | `unsigned] * size *)
        match final with
        | Stage_1.Finally_get_integer (e,s,c) -> (e,s,c)
        | Stage_1.Finally_fail `string ->
          fail compiler "Cannot compile the 'value' of a string/payload" in
      let c_type, c_type_size, cast =
        match signedism with
        | `unsigned -> 
          let c_type, c_type_size = c_value_type_of_size compiler sz in
          (c_type, c_type_size,
           fun e -> `cast (c_type, e))
        | `signed -> to_do "Signed integers"
      in
      let endianise e =
        match endianism with
        | `big -> 
          begin match c_type_size with 
          | 8 -> e
          | 16 -> `call (`variable "ntohs", [e])
          | 32 -> `call (`variable "ntohl", [e])
          | 64 -> to_do "Big endian 64 bit integers"
          | _ -> fail compiler "c_type_size not in {8, 16, 32, 64}"
          end
        | `little -> to_do "Little endian integers" in
      let the_bits =
        match bit_offset, sz with
        | Size_fixed bofs, Size_fixed psz ->
          begin match bofs + psz with
          | 0 -> `literal_int 0
          | s when 1 <= s && s <= 32 ->
            let pointer_expr = Typed_expression.expression pointer in
            let value_at_pointer =
              `unary (`unary_memof, `cast (`pointer c_type, pointer_expr)) in
            let endianised_value =
              `cast (c_type, endianise value_at_pointer) in
            let aligned =
              `binary (`bin_shr, endianised_value,
                       `literal_int (c_type_size - bofs - psz)) in
            `binary (`bin_band, aligned, Construct.ones_int_literal psz)
          | s ->
            to_do (sprintf "Integer's offset + size = %d (>= 32)" s)
          end
        | _, _ ->
          to_do "Integer's offset or size not resolved to constants"
      in
      (Typed_expression.create ~expression:the_bits ~c_type (), c_type_size)
      
    let compile_expression compiler packet expression = 
      let (field, 
           byte_offset,
           bit_offset,
           final,
           dependencies,
           needed_as_by) =
        Stage_1.explode_stage_1_compiled_expression expression in
      set_location_field compiler field;
      let compile_value_to, compile_offset_to, compile_pointer_to =
        let value_needness, offset_needness, pointer_needness =
          Ls.fold_left ~f:(fun (v, o, p) nab ->
            match nab with
            | `value _ -> (1 + v, o, p)
            | `offset _ -> (v, o + 1, p)
            | `pointer _ -> (v, o, p + 1))
            ~init:(0, 0, 0) needed_as_by in
        let comp n = match n, compiler.variable_creation_preference with
          | 0, _ -> `none
          | _, `for_all -> `variable
          | 1, `as_needed -> `expression
          | n, `minimalistically -> `expression
          | n, `as_needed -> `variable
        in
        (comp value_needness, comp offset_needness, comp pointer_needness) in
      let c_offset = c_offset compiler byte_offset in
      let c_pointer = c_pointer_in_packet compiler packet byte_offset in

      let compiled_offset =
        match compile_offset_to with
        | `none -> C_not_compiled
        | `expression -> C_expression c_offset
        | `variable ->
          let c_type = Typed_expression.c_type c_offset in
          let c_var =
            Variable.create ~name:(sprintf "offset_of_%s" field) ~c_type () in
          C_variable (c_var, c_offset)
      in
      let compiled_pointer =
        match compile_pointer_to with
        | `none -> C_not_compiled
        | `expression -> C_expression c_pointer
        | `variable ->
          let c_type = Typed_expression.c_type c_pointer in
          let c_var =
            Variable.create ~name:(sprintf "pointer_to_%s" field) ~c_type () in
          C_variable (c_var, c_pointer)
      in
      let compiled_value, value_size =
        match compile_value_to with
        | `none -> (C_not_compiled, 0)
        | `expression ->
          let c_val, size = c_value compiler c_pointer bit_offset final in
          (C_expression c_val, size)
        | `variable ->
          let c_val, size = c_value compiler c_pointer bit_offset final in
          let c_type = Typed_expression.c_type c_val in
          let c_var = 
            Variable.create ~name:(sprintf "value_of_%s" field) ~c_type () in
          (C_variable (c_var, c_val), size)
      in
      let entity =
        {c_value = compiled_value;
         c_offset = compiled_offset;
         c_pointer = compiled_pointer;
         stage_1_expression = expression;
         buffer_access =
            (Typed_expression.expression c_offset, value_size);} in
      Ht.add compiler.compiled_entities field entity;
      entity

    let generate_constant_size_checks compiler entities
        size_expression escape_block =
      let maximal_constant_access=
        Ls.fold_left ~init:(-1) entities ~f:(fun m e ->
          match e.buffer_access with
          | (`literal_int i, s) -> max m (i + (s / 8)) | _ -> m) in
      debug$ sprintf "Maximal: %d" maximal_constant_access;
      let c_size = Typed_expression.expression size_expression in
      let stop_now, c_size_is_literal =
        match c_size with
        | `literal_int i -> (i < maximal_constant_access, true)
        | `literal_int64 i64 ->
          ((Int64.compare i64 (Int64.of_int maximal_constant_access)) <= 0,
           true)
        | `literal_float f -> (f < (float maximal_constant_access), true)
        | `literal_char c -> ((int_of_char c) < maximal_constant_access, true)
        | _ -> (false, false)
      in
      if stop_now then
        error compiler
          (sprintf  "Found statically that the packet size is inferior \
                     to the maximal buffer access: %s < %d"
             (C_to_str.expression c_size) maximal_constant_access);
      let statements =
        if (maximal_constant_access <> -1) && (not c_size_is_literal) then
          [ `comment "Checking maximal_constant_access against packet size.";
            (Construct.if_then_else ~block_then:escape_block
               (`binary (`bin_le, c_size,
                         `literal_int maximal_constant_access)))]
        else [] in
      Construct.block ~statements ()



    let generate_size_check compiler entity size_expr_opt escape_block =
      match size_expr_opt, entity.buffer_access with
      | None, _  | _ , (`literal_int _, _) -> Construct.block ()
      | Some size_expr, (offset_expr, type_bits) ->
        let c_size = Typed_expression.expression size_expr in
        let bufacc_expr = 
          `binary (`bin_add, offset_expr, `literal_int (type_bits / 8)) in
        let statements = 
          [`comment (sprintf 
                       "Checking buffer-access of field %s against packet size."
                       entity.stage_1_expression.Stage_1.s1_field);
           Construct.if_then_else (`binary (`bin_le, c_size, bufacc_expr))
             ~block_then:escape_block; ] in
        Construct.block ~statements ()
        

    let get_variables_and_size_checks
        compiler entities size_expression_option escape_block  = 
      let decls = ref [] in
      let assigns = ref [] in
      let final_checks = ref [] in
      Ls.iter (fun c ->
        let check_decl, check_stms = 
          generate_size_check compiler c size_expression_option escape_block in
        let is_a_variable = function C_variable _ -> true | _ -> false in
        let there_are_variables =
          is_a_variable c.c_value
          || is_a_variable c.c_offset
          || is_a_variable c.c_pointer in
        decls := !decls @ check_decl;
        if there_are_variables then (
          assigns := !assigns @ check_stms;
        ) else (
          final_checks := !final_checks @ check_stms;
        );
        let add = function 
          | C_variable (v, e) -> 
            decls := !decls @ [(Variable.declaration v)];
            assigns := !assigns @ 
              [Variable.assignment v (Typed_expression.expression e)];
          | _ -> ()
        in
        add c.c_value;
        add c.c_offset;
        add c.c_pointer;
      ) entities;
      (!decls, !assigns @ !final_checks)


    (* 'Exported' function: *)

    let informed_block ~stage_1 ?(platform=Promiwag_platform.default)
        ?(create_variables:variable_creation_preference=`as_needed)
        ~(packet:c_packet)
        ?(size_error_block=([], [`comment "Default size_error_block."]))
        ~(make_user_block: Typed_expression.t list -> C.block) () =
      let compiler = 
        {stage_1 = stage_1; variable_creation_preference = create_variables;
         compiled_entities = Ht.create 42; target_platform = platform;
         location = None} in
     
      let ordered_entities =
        Ls.map compiler.stage_1.Stage_1.compiled_expressions 
          ~f:(compile_expression compiler packet.packet_expression) in

      compiler.location <- 
        Some "{Generating constant size/buffer-access checks}";
      let cstszchecks_declarations, cstszchecks_statements =
        match packet.packet_size with
        | None -> ([], [])
        | Some var ->
          generate_constant_size_checks
            compiler ordered_entities var size_error_block in

      compiler.location <- Some "{Generating declarations, assignments, and \
                                  size/buffer-access checks}";
      let declarations, assignments = 
        get_variables_and_size_checks compiler ordered_entities 
          packet.packet_size size_error_block in
      
      compiler.location <- Some "{Generating user_expressions}";
      let user_expressions =
        Ls.map compiler.stage_1.Stage_1.request_list
          ~f:(function
            | `value f -> get_c_dependency compiler `value f
            | `offset f -> get_c_dependency compiler `offset f
            | `pointer f -> get_c_dependency compiler `pointer f) in
      let user_decls, user_stmts = make_user_block user_expressions in
      (cstszchecks_declarations @ declarations @ user_decls,
       cstszchecks_statements @ assignments @ user_stmts)

  end

end



