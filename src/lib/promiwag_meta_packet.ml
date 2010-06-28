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
    | `align (i, e) -> Size_alignment (i, parse_size e)
    | `offset v -> Size_offset_of v
    | `size s -> s

  let size = parse_size

  type content_type =
    | Type_unsigned_integer of size
    | Type_signed_integer of size
    | Type_little_endian of content_type
    | Type_string of size
        
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

  type request = [ `field of string | `offset of string ]

  exception Field_not_found
  exception Max_dependency_depth_reached of int
  exception Error_compile_unknown_size

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
      | Finally_get_pointer

    let string_of_final_computation = function
      | Finally_get_integer (`big, `signed, s) -> 
        sprintf "(get-integer %s, signed, big endian)" (string_of_size s)
      | Finally_get_integer (`big, `unsigned, s) -> 
        sprintf "(get-integer %s, unsigned, big endian)" (string_of_size s)
      | Finally_get_integer (`little, `signed, s) -> 
        sprintf "(get-integer %s, signed, little endian)" (string_of_size s)
      | Finally_get_integer (`little, `unsigned, s) -> 
        sprintf "(get-integer %s, unsigned, little endian)" (string_of_size s)
      | Finally_get_pointer -> "get-pointer"

    let rec final_computation_of_content_type ?(endianism=`big) = function
      | Type_little_endian ct -> 
        final_computation_of_content_type ~endianism:`little ct
      | Type_unsigned_integer sz ->
        (Finally_get_integer (endianism, `unsigned, sz),
         dependencies_of_size sz)
      | Type_signed_integer sz ->
        (Finally_get_integer (endianism, `unsigned, sz),
         dependencies_of_size sz)
      | Type_string sz -> (Finally_get_pointer, [])

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
      | Size_alignment (i, s) ->
        begin match propagate_constants_in_size s with
        | Size_fixed s -> Size_fixed (s + (s mod i))
        | other -> other
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
          raise Field_not_found
        | (Item_field (name, tp)) :: l ->
          if cmp_str name field_name then
            let fcomp, fdeps = final_computation_of_content_type tp in
            (fcomp, fdeps, Ls.rev computations, Ls.rev dependencies)
          else
            let comp, dep = computation_of_content_type tp in
            f (comp :: computations) (dep :: dependencies) field_name l
      in
      (f [] [] field_name packet_format)

    type stage_1_compiled_expression = {  (* TODO find a better name... *)
      s1_field: string;
      s1_byte_offset: size;
      s1_bit_offset: size;
      s1_final: final_computation;
      s1_dependencies: dependency list;
      mutable s1_needed_as: [`value | `offset | `both];
      mutable s1_needed_by:
        [`request | `other of stage_1_compiled_expression] list;
    }

    let explode_stage_1_compiled_expression ce =
      (ce.s1_field, 
       ce.s1_byte_offset,
       ce.s1_bit_offset,
       ce.s1_final,
       ce.s1_dependencies,
       ce.s1_needed_as,
       ce.s1_needed_by)

    let stage_1_compile_field needed_as needed_by packet_format field_name =
      let final_comp, final_deps, comps, deps =
        find_field_in_packet packet_format field_name in
      let total_deps = final_deps @ (Ls.flatten deps) in
      let byte_ofs, bit_ofs = aggregate_computations comps in
      {s1_field = field_name; s1_byte_offset = byte_ofs; s1_bit_offset = bit_ofs;
       s1_final = final_comp; s1_dependencies = total_deps;
       s1_needed_as = needed_as; s1_needed_by = needed_by}

    let stage_1_compile_dependency packet_format needed_by = function
      | Depend_on_value_of f ->
        stage_1_compile_field `value needed_by packet_format f
      | Depend_on_offset_of f ->
        stage_1_compile_field `offset needed_by packet_format f
      | Depend_on_unknown -> fail "stage_1_compile_dependency: should not be here"

    let dependencies_of_request: request list -> dependency list =
      Ls.map ~f:(function 
        | `field f -> Depend_on_value_of f
        | `offset f -> Depend_on_offset_of f)

    type result = {
      packet_format: packet;
      request_list: request list;
      compiled_expressions: stage_1_compiled_expression list;
    }

    let compile_with_dependencies
        ?(max_depth=42) ~packet_format (request:request list) =
      let first_dependencies = dependencies_of_request request in
      let stage_compiled_things = ref [] in
      let die_on_max_depth d =
        if d > max_depth then raise (Max_dependency_depth_reached d); in
      let rec go_deeper depth dependers = function
        | [] -> (* Done! *) ()
        | (Depend_on_value_of fname as d) :: l
        | (Depend_on_offset_of fname as d) :: l ->
          die_on_max_depth depth;
          let f ct = cmp_str ct.s1_field fname in
          begin match Ls.find_all !stage_compiled_things ~f with
          | [] ->
            let compiled =
              stage_1_compile_dependency (snd packet_format) dependers d in
            go_deeper (depth + 1) [ `other compiled ] compiled.s1_dependencies;
            stage_compiled_things := compiled :: !stage_compiled_things;
            go_deeper depth dependers l;
          | one :: [] ->
            begin match one.s1_needed_as, d with
            | `both, _
            | `value, Depend_on_value_of _
            | `offset, Depend_on_offset_of _ -> ()
            | `value, Depend_on_offset_of _
            | `offset, Depend_on_value_of _ ->
              one.s1_needed_as <- `both;
            | _ -> ()
            end;
            one.s1_needed_by <- dependers @ one.s1_needed_by;
          | more -> 
            fail (sprintf "field %s added %d times to stage_compiled_things"
                    fname (Ls.length more))
          end
        | Depend_on_unknown :: l ->
          fail "Stage 1: Cannot compile unknown dependency"
      in
      go_deeper 0 [`request] first_dependencies;
      {packet_format = packet_format; 
       request_list = request;
       compiled_expressions = Ls.rev !stage_compiled_things}

    let string_of_stage_1_compiled_expression
        ?(before="") ?(sep_parens=" ") ce =
      sprintf "%s[\"%s\" (byte_offset: %s, bit_offset: %s)%s\
              (final: %s)%s(dependencies: [%s])%s(needed by: [%s] as %s)]"
        before ce.s1_field
        (string_of_size ce.s1_byte_offset) 
        (string_of_size ce.s1_bit_offset) sep_parens
        (string_of_final_computation ce.s1_final) sep_parens
        (Str.concat "; " (Ls.map string_of_dependency ce.s1_dependencies))
        sep_parens
        (Str.concat "; " (Ls.map (function
          | `request -> "request" | `other c -> c.s1_field) ce.s1_needed_by))
        (match ce.s1_needed_as with
        | `value -> "value" | `offset -> "offset" | `both -> "both")

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
          | `field f -> sprintf "field %s" f
          | `offset f -> sprintf "offset of %s" f) s1.request_list))
        (fst s1.packet_format)
        (string_of_stage_1_compilation_ht
           ~before:"  " ~sep_parens:"\n    " s1.compiled_expressions)

  end

  module Stage_2_C = struct
    
    module C = Promiwag_c_backend.C_LightAST
    open Promiwag_c_backend
    open Packet_structure
    let error_prefix = "Meta_packet.Parser_generator.Stage_2_C"
    let to_do s =
      failwith (sprintf "%s; %s: NOT IMPLEMENTED" error_prefix s)
    let fail s =
      failwith (sprintf "%s: ERROR %s" error_prefix s)

    type compiled =
      (* TODO: extend with more types ! and platform specificities *)
      | C_value_variable of Variable.t * Typed_expression.t
      | C_offset_variable of Variable.t * Typed_expression.t
      | C_value_expression of Typed_expression.t * Typed_expression.t
      | C_offset_expression of Typed_expression.t

    type compiler = {
      stage_1: Stage_1.result;
      c_dependencies: (string, compiled) Ht.t;
      target_platform: Promiwag_platform.platform;
    }

    let get_c_dependency compiler needed_as name =
      let c_type =
        match needed_as with
        | `value -> Promiwag_platform.C.native_uint compiler.target_platform
        | `offset -> `pointer `void in
      match Ht.find_opt compiler.c_dependencies name, needed_as with
      | Some (C_value_variable (v, ev)), `value -> Variable.typed_expression v
      | Some (C_value_variable (v, ev)), `offset ->
        Typed_expression.create () ~c_type:(`pointer `void) 
          ~expression:(`cast (c_type, 
                              (`unary (`unary_addrof, Variable.expression v))))
      | Some (C_offset_variable (v, e)), `offset -> Variable.typed_expression v
      | Some (C_offset_variable (v, e)), `value -> 
        fail "asking for the value at a pointer at the wrong place"
      | Some (C_value_expression (ev, eo)), `value -> eo
      | Some (C_value_expression (ev, eo)), `offset -> ev
      | Some (C_offset_expression e), `offset -> e
      | Some (C_offset_expression e), `value -> 
        fail "asking for the value at a pointer at the wrong place"
      | None, _ ->
        fail (sprintf "Can't find dependency \"%s\"" name)

    let get_c_dependency_expression c n a =
      Typed_expression.expression (get_c_dependency c n a)

    let get_variables compiler = 
      let decls = ref [] in
      let assigns = ref [] in
      Ht.iter (fun _ dep ->
        match dep with
        | C_value_variable (v, e)
        | C_offset_variable (v, e) -> 
          decls := (Variable.declaration v) :: !decls;
          assigns := 
            (Variable.assignment v (Typed_expression.expression e)) :: !assigns;
        | _ -> ()) compiler.c_dependencies;
      (Ls.rev !decls, Ls.rev !assigns)

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
        to_do "Size alignment"
      | Size_offset_of v ->
        get_c_dependency_expression compiler needed_as v
      | Size_unknown -> 
        raise Error_compile_unknown_size

    let c_offset_in_packet compiler packet byte_offset =
      let packet_as_buffer = 
        `cast (`pointer `unsigned_char,
               Typed_expression.expression packet) in
      let c_offset = 
        `cast (`pointer `unsigned_char,
               compile_size compiler `value byte_offset) in
      let the_address_at_offset =
        `binary (`bin_add, packet_as_buffer, c_offset) in
      Typed_expression.create ~expression:the_address_at_offset
        ~c_type:(`pointer `void) ()
        
    let c_value_of_offset_pointer compiler pointer bit_offset = function
      | Stage_1.Finally_get_pointer -> pointer
      | Stage_1.Finally_get_integer (endianism, signedism, sz) ->
        (* [`big | `little] * [`signed | `unsigned] * size *)
        let endianise e =
          match endianism with
          | `big -> `call (`variable "ntohl", [e])
          | `little -> to_do "Little endian integers" in
        let c_type, cast =
          match signedism with
          | `unsigned -> 
            (`unsigned_int,
             fun e -> `cast (`unsigned_int, `unary (`unary_memof, e)))
          | `signed ->
            (* emit a warning? *) 
            (`signed_int, fun e -> `cast (`signed_int, `unary (`unary_memof, e)))
        in
        let the_bits =
          match bit_offset, sz with
          | Size_fixed bofs, Size_fixed psz ->
            begin match bofs + psz with
            | 0 -> `literal_int 0
            | s when 1 <= s && s <= 32 ->
              let aligned =
                `binary (`bin_shr, 
                         endianise (cast (Typed_expression.expression pointer)),
                         `literal_int (32 - bofs - psz)) in
              `binary (`bin_band, aligned, Construct.ones_int_literal psz)
            | s ->
              to_do (sprintf "Integer's offset + size = %d (>= 32)" s)
            end
          | _, _ ->
            to_do "Integer's offset or size not resolved to constants"
        in
        (Typed_expression.create ~expression:the_bits ~c_type ())
          
    let compile_expression compiler packet expression = 
      let (field, 
           byte_offset,
           bit_offset,
           final,
           dependencies,
           needed_as,
           needed_by) =
        Stage_1.explode_stage_1_compiled_expression expression in
      let compile_to =
        let needness = Ls.length needed_by in
        match needness with
        | 0 -> fail (sprintf "field %s needed by no one?" field)
        | 1 -> `expression
        | n -> `variable  in
      let compile_as = 
        match needed_as with
        | `value | `both -> `value
        | `offset -> `offset in
      debug$ sprintf "Compiling %s's" field;
      let c_offset = c_offset_in_packet compiler packet byte_offset in
      debug$ sprintf " C-offset: %s"
        (code$  C_to_str.expression (Typed_expression.expression c_offset));

      let compiled_thing =
        match compile_as with
        | `value ->
          let c_val =
            c_value_of_offset_pointer compiler c_offset bit_offset final in
          debug$ sprintf " to value %s" 
            (code$  C_to_str.expression (Typed_expression.expression c_val));
          begin match compile_to with
          | `expression ->
            C_value_expression (c_val, c_offset)
          | `variable ->
            let c_type = `unsigned_int in
            let c_var = Variable.create ~name:field ~c_type () in
            C_value_variable (c_var, c_val)            
          end
        | `offset ->
          begin match compile_to with
          | `expression ->
            C_offset_expression c_offset
          | `variable ->
            let c_type = `pointer `void in
            let c_var =
              Variable.create ~name:(sprintf "%s_addr" field) ~c_type () in
            C_offset_variable (c_var, c_offset)
          end
      in
      Ht.add compiler.c_dependencies field compiled_thing;
      field

    let informed_block ~stage_1 ~platform
        ~(packet_expression: Typed_expression.t)
        ~(make_user_block: Typed_expression.t list -> C.block) =
      let compiler = 
        {stage_1 = stage_1;
         c_dependencies = Ht.create 42;
         target_platform = platform} in

      let _compiled =
        Ls.map compiler.stage_1.Stage_1.compiled_expressions 
          ~f:(compile_expression compiler packet_expression) in
      let declarations, assignments = get_variables compiler in
      let user_expressions =
        Ls.map compiler.stage_1.Stage_1.request_list
          ~f:(function
            | `field f -> get_c_dependency compiler `value f
            | `offset f -> get_c_dependency compiler `offset f) in
      let user_decls, user_stmts = make_user_block user_expressions in
      (declarations @ user_decls, assignments @ user_stmts)

  end

end



