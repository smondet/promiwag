open Promiwag_std

module Packet_structure = struct

  type int_operator =
    | Op_add
    | Op_sub
    | Op_mul
    | Op_div

  let caml_op_of_size_op = function
    | Op_add -> (+) | Op_sub -> (-) | Op_mul -> ( * ) | Op_div -> (/)

  type size =
    | Size_fixed of int
    | Size_variable of string
    | Size_binary_expression of int_operator * size * size
    | Size_alignment of int * size
    | Size_offset_of of string
    | Size_unknown

  let size_int i = Size_fixed i
  let size_var v = Size_variable v

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

  type t = (string, Packet_structure.packet) Hashtbl.t

  let add t (name, packet) = Hashtbl.add t name (name, packet)

  let of_list l = 
    let t = Hashtbl.create 42 in
    List.iter (fun p -> add t p) l;
    t

  let get_format (t:t) name =
    try let _, fmt = Hashtbl.find t name in fmt with
    | Not_found ->
      failwith (sprintf "ERROR: Meta_packet.Packet_database.find \
                   packet format \"%s\" not found" name)

end

module C_parsing = struct

  module C = Promiwag_c_backend.C_LightAST
  module Cons = Promiwag_c_backend.Construct
  open Promiwag_c_backend

  type request = [ `field of string ]

  exception Field_not_found
  exception Max_dependency_depth_reached of int
  exception Error_compile_unknown_size

  module Internal = struct 

    open Packet_structure

    let cmp_str a b = (String.compare a b) = 0
    let to_do s =
      printf "%!";
      failwith (sprintf "Meta_packet.C_parsing.Internal: \
                           %s: NOT IMPLEMENTED" s)
        
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

    type final_computation =
      | Finally_get_integer of [`big | `little] * [`signed | `unsigned] * size
      | Finally_get_pointer

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

    let find_field_in_packet packet_format field_name =
      let rec f computations field_name = function
        | [] ->
          raise Field_not_found
        | (Item_field (name, tp)) :: l ->
          if cmp_str name field_name then
            (final_computation_of_content_type tp, List.rev computations)
          else
            let comp = computation_of_content_type tp in
            f (comp :: computations) field_name l
      in
      (f [] field_name packet_format)

        
    let resolve_dependencies max_depth packet_format (request:request list) =
      let request_as_dependencies =
        List.map (function `field f -> Depend_on_value_of f) request in
      let computable_variables = Hashtbl.create 42 in
      let die_on_max_depth d =
        if d > max_depth then raise (Max_dependency_depth_reached d); in
      let rec go_deeper ?(depth=0) = function
        | [] -> (* Done! *) ()
        | Depend_on_value_of f :: l ->
          die_on_max_depth depth;
          begin match Hashtbl.find_all computable_variables (`value f) with
          | [] ->
            let (final_comp, final_deps) as final, computes =
              find_field_in_packet packet_format f in
            let deps = 
              final_deps @ 
                (List.flatten (List.map (fun (_, d) -> d) computes)) in
            go_deeper ~depth:(depth + 1) deps;
            Hashtbl.add computable_variables (`value f) (final, computes, deps);
            go_deeper ~depth:(depth + 1) l;
          | one :: [] -> ()
          | more -> 
            failwith (sprintf "field %s added %d times to \
                                 computable_variables as `value"
                        f (List.length more))
          end
        | Depend_on_offset_of f :: l -> 
          die_on_max_depth depth;
          begin match Hashtbl.find_all computable_variables (`offset f) with
          | [] ->
            let (final_comp, final_deps) as final, computes =
              find_field_in_packet packet_format f in
            let deps = 
              (List.flatten (List.map (fun (_, d) -> d) computes)) in
            go_deeper ~depth:(depth + 1) deps;
            Hashtbl.add computable_variables (`offset f) (final, computes, deps);
            go_deeper ~depth:(depth + 1) l;
          | one :: [] -> ()
          | more -> 
            failwith (sprintf "field %s added %d times to \
                                 computable_variables as `offset"
                        f (List.length more))
          end
        | Depend_on_unknown :: _ ->
          failwith "Depends on something Unknown/Uncomputable"
      in
      go_deeper request_as_dependencies;
      computable_variables

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

    (* ** Generic / C limit ** *)
    let c_op_of_size_op = function
      | Op_add -> `bin_add
      | Op_sub -> `bin_sub
      | Op_mul -> `bin_mul
      | Op_div -> `bin_div

    let rec compile_size = function
      | Size_fixed s -> `literal_int s
      | Size_variable v -> `variable v
      | Size_binary_expression (op, size_a, size_b) ->
        let compiled_a, compiled_b = compile_size size_a, compile_size size_b in
        `binary (c_op_of_size_op op, compiled_a, compiled_b)
      | Size_alignment (i, s) ->
        to_do "Size alignment"
      | Size_offset_of v ->
        (`unary (`unary_addrof, `variable v))
      | Size_unknown -> 
        raise Error_compile_unknown_size

    let c_offset_of_packet packet byte_offset =
      let packet_as_buffer = 
        `cast (`pointer `unsigned_char, Typed_expression.expression packet) in
      let the_address_at_offset =
        `binary (`bin_add, packet_as_buffer, compile_size byte_offset) in
      Typed_expression.create ~expression:the_address_at_offset
        ~c_type:(`pointer `void) ()

    let c_value_of_offset_pointer pointer bit_offset final =
      match final with
      | Finally_get_pointer -> (* and the bit_offset? *)
        (Typed_expression.create ~expression:pointer ~c_type:(`pointer `void) (),
         Typed_expression.create ~expression:pointer ~c_type:(`pointer `void) ())
      | Finally_get_integer (endianism, signedism, sz) ->
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
        let propagated_size = propagate_constants_in_size sz in
        let the_bits =
          match bit_offset, propagated_size with
          | Size_fixed bofs, Size_fixed psz ->
            begin match bofs + psz with
            | 0 -> `literal_int 0
            | s when 1 <= s && s <= 32 ->
              let aligned =
                `binary (`bin_shr, endianise (cast pointer),
                         `literal_int (32 - bofs - psz)) in
              `binary (`bin_band, aligned, Construct.ones_int_literal psz)
            | s ->
              to_do (sprintf "Integer's offset + size = %d (>= 32)" s)
            end
          | _, _ ->
            to_do "Integer's offset or size not resolved to constants"
        in
        (Typed_expression.create ~expression:the_bits ~c_type (),
         Typed_expression.create ~expression:pointer ~c_type:(`pointer `void) ())
          

    type compiled_chunk = 
      | Compiled_value_expression of 
          Typed_expression.t * Typed_expression.t
      | Compiled_offset_expression of Typed_expression.t
      | Compiled_value_variable of
          Variable.t * Typed_expression.t * Typed_expression.t
      | Compiled_offset_variable of Variable.t * Typed_expression.t
        
    let compile_offset already_compiled final computes name packet =
      (* assert (final = (Finally_get_pointer, [])); *)
      let computations = List.map fst computes in
      printf "Offset of %s " name;
      begin match Ht.find_opt already_compiled name with
      | Some _ -> printf "is already compiled somewhere\n";
      | None ->
        let byte_ofs, bit_ofs = aggregate_computations computations in
        if bit_ofs <> Size_fixed 0 then (
          to_do "Bit offset of a pointer in [1 .. 7]";
        ) else (
          let expr = c_offset_of_packet packet byte_ofs in
          Ht.add already_compiled name (Compiled_offset_expression expr);
          printf "will be: %s\n" (C_to_str.expression 
                                    (Typed_expression.expression expr));
        );
      end

    let compile_value already_compiled final computes name packet =
      printf "Value of %s" name;
      let value_and_offset_expressions expr = 
        let computations = List.map fst computes in
        let byte_ofs, bit_ofs = aggregate_computations computations in
        c_value_of_offset_pointer expr bit_ofs (fst final) in
      begin match Ht.find_opt already_compiled name with
      | Some (Compiled_value_variable _)
      | Some (Compiled_value_expression _) ->
        printf "is already compiled somewhere\n";
      | Some (Compiled_offset_variable (v, e)) ->
        printf " has an offset variable \"%s = %s\","
          (C_to_str.expression (Variable.expression v))
          (C_to_str.expression (Typed_expression.expression e));
        let val_expr, ofs_expr =
          value_and_offset_expressions (Variable.expression v) in
        printf " so, its value is \"%s\"" 
          (C_to_str.expression (Typed_expression.expression val_expr));
        Ht.add already_compiled name 
          (Compiled_value_expression (val_expr, ofs_expr))
      | Some (Compiled_offset_expression e) ->
        printf " has an offset expression \"%s\","
          (C_to_str.expression (Typed_expression.expression e));
        let val_expr, ofs_expr =
          value_and_offset_expressions (Typed_expression.expression e) in
        printf " hance, its value is \"%s\"" 
          (C_to_str.expression (Typed_expression.expression val_expr));
        Ht.add already_compiled name 
          (Compiled_value_expression (val_expr, ofs_expr))
      | None ->
        printf " has to be compiled from scratch,";
        let computations = List.map fst computes in
        let byte_ofs, bit_ofs = aggregate_computations computations in
        let expr = c_offset_of_packet packet byte_ofs in
        printf " so its offset will be \"%s\""
          (C_to_str.expression (Typed_expression.expression expr));
        let val_expr, ofs_expr =
          value_and_offset_expressions (Typed_expression.expression expr) in
        printf " therefore its value is \"%s\"" 
          (C_to_str.expression (Typed_expression.expression val_expr));
        Ht.add already_compiled name 
          (Compiled_value_expression (val_expr, ofs_expr))
      end;
      (printf "\n")

    let compile computables_ht request packet_expression =
      let compiled_chunks = Ht.create 42 in
      Ht.iter (fun f  (final, computes, deps) ->
        match f with
        | `offset s ->
          compile_offset compiled_chunks final computes s packet_expression
        | `value s -> 
          compile_value compiled_chunks final computes s packet_expression
      ) computables_ht;

      let f = function 
        | `field field ->
          begin match Ht.find_opt compiled_chunks field with
          | Some (Compiled_value_expression (e, _)) -> e
          | Some (Compiled_value_variable (v, e, _)) -> Variable.typed_expression v
          | Some (Compiled_offset_expression _)  
          | None
          | Some (Compiled_offset_variable _) ->
            failwith (sprintf "Field %s should have been compiled!" field)
          end
      in
      Ls.map request ~f

  end


  let informed_block
      ~(packet_format:Packet_structure.format)
      ~(request_list: request list)
      ~(packet_expression: Typed_expression.t)
      (* ?(packet_size: Typed_expression.t option)  TODO: maybe something else? *)
      ~(make_user_block: Typed_expression.t list -> C.block) =
      (*
        - for each field 'compile' it to: (computation, dependencies)
        - compute dependencies (Hashtbl / Lazy style)
        - if something does not seem computable -> fail
        - else:
        - for dependencies which are needed more than once create local variable
        - generate C code for every computation
      *)
    
    let computables = 
      Internal.resolve_dependencies 42 packet_format request_list in

    let compiled_c_expressions = 
      Internal.compile computables request_list packet_expression in

    let user_decl, user_stms = make_user_block compiled_c_expressions in
    (user_decl, user_stms)



end



